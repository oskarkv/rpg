(ns game.client.core
  (:import (com.jme3.system AppSettings JmeContext$Type)
           (com.jme3.app FlyCamAppState))
  (:require [game.networking.core :as net]
            [game.game-map :as gmap]
            [game.constants :as consts]
            (game.client [input :as c-input]
                         [hud :as hud])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [input :as cmn-input]
                         [graphics :as gfx])
            [game.math :as gmath]
            [clojure.math.numeric-tower :as math])
  (:use game.utils))

(defmulti process-msg (fn [game-state msg] (:type msg)))

(defmethod process-msg :s-login [game-state {:keys [id player]}]
  {:new-game-state (assoc-in game-state [:chars id] player)})

(defmethod process-msg :s-own-id [game-state {id :id}]
  {:new-game-state (assoc game-state :own-id id)})

(defmethod process-msg :s-game-state [game-state {new-game-state :game-state}]
  {:new-game-state (merge game-state new-game-state)})

(defmethod process-msg :s-move [game-state {:keys [positions]}]
  (let [positions (dissoc positions (:own-id game-state))]
    {:new-game-state
     (reduce (fn [gs [id pos]] (assoc-in gs [:chars id :new-pos] pos))
             game-state
             positions)}))

(defmethod process-msg :s-char-death [game-state {:keys [id]}]
  {:new-game-state (dissoc-in game-state [:chars id])})

(defmethod process-msg :s-spawn-corpse [game-state {:keys [id-corpse]}]
  {:new-game-state (update-in game-state [:corpses] conj id-corpse)})

(defmethod process-msg :s-spawn-player [game-state {:keys [id-char]}]
  {:new-game-state (update-in game-state [:chars] conj id-char)})

(defmethod process-msg :s-attack [game-state {:keys [target damage]}]
  {:new-game-state (update-in game-state [:chars target :hp] - damage)})

(defmethod process-msg :s-spawn-mobs [game-state {:keys [mobs]}]
  {:new-game-state (update-in game-state [:chars] merge mobs)})

(defmethod process-msg :s-decay-corpses [game-state {:keys [ids]}]
  {:new-game-state (update-in game-state [:corpses] #(apply dissoc % ids))})

(defmethod process-msg :s-loot [game-state {:keys [drops]}])

(defmethod process-msg :default [game-state msg])

(defmulti produce-server-msg (fn [_ event] (:type event)))

(defmethod produce-server-msg :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:chars own-id])
        pos (map float (:pos self))
        dir (map float (:move-dir self))]
    {:type :c-move :pos pos :move-dir dir}))

(defmethod produce-server-msg :toggle-attack [game-state event]
  {:type :c-toggle-attack})

(defmethod produce-server-msg :target [game-state event]
  (assoc event :type :c-target))

(defmethod produce-server-msg :loot [{:keys [own-id] :as game-state} event]
  (when (ccfns/close-enough? game-state own-id (:corpse event)
                             consts/loot-distance)
    (assoc event :type :c-loot-corpse)))

(defn process-network-msgs [game-state net-map]
  (ccfns/process-network-msgs game-state net-map process-msg))

(defn calculate-movement-direction [key-state]
  (let [[x y] (gfx/get-camera-dir)
        adder (fn [dx dy] (fn [[sx sy]] [(+ dx sx) (+ dy sy)]))]
    (cond-> [0 0]
      (:forward key-state) ((adder x y))
      (:back key-state) ((adder (- x) (- y)))
      (:left key-state) ((adder (- y) x))
      (:right key-state) ((adder y (- x)))
      true (gmath/normalize))))

(defmulti process-tap (fn [_ type] type))

(defmethod process-tap :toggle-attack [{id :own-id :as game-state} _]
  {:new-game-state (update-in game-state [:chars id :attacking] not)
   :event {:type :toggle-attack}})

(defmethod process-tap :target [game-state _]
  (let [id (:own-id game-state)
        target (gfx/pick-target :chars)]
    (when target
      {:new-game-state (assoc-in game-state [:chars id :target] target)
       :event {:type :target :target target}})))

(defmethod process-tap :loot [game-state _]
  (when-let [corpse (gfx/pick-target :corpses)]
    {:event {:type :loot :corpse corpse}}))

(defn process-player-input [game-state key-state]
  (let [id (:own-id game-state)
        {:keys [new-game-state events]} (ccfns/process-player-input
                                         game-state key-state process-tap)
        old-dir (map float (get-in new-game-state [:chars id :move-dir]))
        new-dir (map float (calculate-movement-direction key-state))
        new-game-state (assoc-in new-game-state [:chars id :move-dir] new-dir)
        events (if (= old-dir new-dir) events (conj events {:type :new-dir}))]
    {:new-game-state new-game-state :events events}))

(defn process-received-game-state [game-state]
  (let [curr-time (current-time-ms)]
    (assoc game-state :last-move curr-time)))

(defn login-and-recv-state [game-state net-map name password stop?]
  (let [{:keys [net-sys send-msg get-msg]} net-map]
    (send-msg {:type :c-login :username name :password password})
    (loop [game-state game-state]
      (let [new-game-state
            (:new-game-state (process-network-msgs game-state net-map))]
        (if (let [id (:own-id new-game-state)]
              (or @stop? (and id (get-in new-game-state [:chars id]))))
          (process-received-game-state new-game-state)
          (recur new-game-state))))))

(defn legal-pos? [{:keys [terrain] :as game-state} pos]
  (let [r consts/player-radius
        -r (- r)
        adder (fn [v] (map + pos v))]
    (every? gmap/walkable-type?
            (map #(get-in terrain (mapv (comp int math/floor) %))
                 (map adder [[r r] [r -r] [-r r] [-r -r]])))))

(defn move-self [{:keys [own-id chars move-time-delta] :as game-state}]
  (let [{:keys [pos move-dir speed] :as self} (chars own-id)
        new-pos (gmath/extrapolate-pos pos move-dir move-time-delta speed)]
    (if (legal-pos? game-state new-pos)
      {:new-game-state (assoc-in game-state [:chars own-id :pos] new-pos)}
      {:new-game-state (assoc-in game-state [:chars own-id :move-dir] [0 0])
       :event {:type :new-dir}})))

(defn move-toward-new-pos [{:keys [pos new-pos speed] :as char} time-delta _]
  (if new-pos
    (let [new-char (ccfns/move-toward-pos char time-delta new-pos)]
      (if (= (:pos new-char) new-pos)
        (dissoc new-char :new-pos)
        new-char))
    char))

(defn move-chars [game-state]
  (let [{:keys [chars move-time-delta last-move]} game-state]
    {:new-game-state
     (assoc-in game-state [:chars]
               (fmap #(move-toward-new-pos % move-time-delta last-move)
                     chars))}))

(defrecord Client [net-map app]
  cc/Lifecycle
  (start [this]
    (cc/start (:net-sys net-map))
    (cc/start app)
    this)
  (stop [this]
    (cc/stop app)
    (cc/stop (:net-sys net-map))
    this))

(defn create-client-jme3-app [net-map game-state-atom]
  (let [stop? (atom false)
        key-bindings (c-input/load-key-bindings)
        key-state-atom (atom (cmn-input/create-key-state-map key-bindings))
        graphics-system (atom nil)
        hud-system (atom nil)
        init-gfx-fn
        (fn [app]
          (gfx/init-graphics-system app (:terrain @game-state-atom)))
        start-input-fn
        (fn [input-manager]
          (cmn-input/start-input
            input-manager
            key-bindings
            key-state-atom))
        simple-init-fn
        (fn [app]
          (let [state-manager (.getStateManager app)
                input-manager (.getInputManager app)
                asset-manager (.getAssetManager app)
                root-node (.getRootNode app)]
            (.detach state-manager (.getState state-manager
                                              FlyCamAppState))
            (.setCursorVisible input-manager true)
            (start-input-fn input-manager)
            (reset! graphics-system (init-gfx-fn app))
            (cc/start @graphics-system)
            (reset! hud-system (hud/init-hud-system app))
            (cc/start @hud-system)
            (reset! game-state-atom
                    (login-and-recv-state @game-state-atom net-map
                                          "leif" "star" stop?))
            (cc/update @graphics-system @game-state-atom)
            (gfx/set-up-camera @game-state-atom)))
        simple-update-fn
        (fn []
          (Thread/sleep 1)
          (let [{:keys [new-game-state events]}
                (ccfns/call-update-fns @game-state-atom [] nil
                  (process-player-input @key-state-atom)
                  (process-network-msgs net-map)
                  (ccfns/calculate-move-time-delta)
                  (move-self)
                  (move-chars))
                to-server-msgs (->> events
                                    (map (partial produce-server-msg
                                                  new-game-state))
                                    (remove nil?))
                send-to-server (:send-msg net-map)]
            (cmn-input/empty-taps key-state-atom)
            (doseq [msg to-server-msgs]
              (send-to-server msg))
            (cc/update @graphics-system new-game-state)
            (cc/update @hud-system new-game-state)
            (reset! game-state-atom new-game-state)))
        init-app-settings-fn
        (fn [app]
          (doto app
            (.setShowSettings false)
            (.setSettings
              (doto (AppSettings. true)
                (.setResolution consts/resolution-x consts/resolution-y)))
            (.setPauseOnLostFocus false)))
        start-fn
        (fn [this]
          (.start this))
        stop-fn
        (fn [this]
          (reset! stop? true)
          (cc/stop @graphics-system)
          (.stop this))]
    (ccfns/create-jme3-app start-fn stop-fn
                           simple-init-fn simple-update-fn
                           init-app-settings-fn)))

(defn create-non-jme3-app [net-map game-state-atom]
  (let [stop? (atom false)]
    (reify cc/Lifecycle
      (start [this]
        (error-printing-future
          (reset! game-state-atom
                  (login-and-recv-state
                    @game-state-atom net-map "leif" "star"
                    stop?))
          ((fn []
             (reset! game-state-atom
                     (:new-game-state
                       (process-network-msgs @game-state-atom net-map)))
             (if @stop? nil (recur))))))
      (stop [this]
        (reset! stop? true)))))

(defn init-client [address port headless]
  (let [game-state-atom (-> (gmap/load-game-map) (dissoc :spawns) atom)
        {:keys [net-sys get-msg send-msg]}
        (net/construct-client-net-sys address port
                                      cc/connect-msg
                                      cc/disconnect-msg)
        new-get-msg (fn []
                      (when-let [msg (get-msg)]
                        (ccfns/msg->map msg)))
        new-send-msg (fn [msg-map]
                       (send-msg (ccfns/map->msg msg-map)))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        app (if headless
              (create-non-jme3-app net-map game-state-atom)
              (create-client-jme3-app net-map game-state-atom))]
    (->Client net-map app)))
