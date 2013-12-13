(ns game.client.core
  (:import (com.jme3.system JmeContext$Type)
           (com.jme3.app FlyCamAppState))
  (:require [game.networking.core :as net]
            [game.game-map :as gmap]
            (game.client [input :as c-input])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [input :as cmn-input]
                         [graphics :as gfx])
            [game.math :as math])
  (:use game.utils))

(defmulti process-msg (fn [game-state msg] (:type msg)))

(defmethod process-msg :s-login [game-state {:keys [id player]}]
  {:new-game-state (assoc-in game-state [:chars id] player)})

(defmethod process-msg :s-own-id [game-state {id :id}]
  {:new-game-state (assoc game-state :own-id id)})

(defmethod process-msg :s-game-state [game-state {new-game-state :game-state}]
  {:new-game-state (merge game-state new-game-state)})

(defmethod process-msg :s-move [game-state {:keys [id pos]}]
  {:new-game-state (assoc-in game-state [:chars id :new-pos] pos)})

(defmethod process-msg :default [game-state _]
  {:new-game-state game-state})

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

(defn process-network-msgs [game-state net-map]
  (ccfns/process-network-msgs game-state net-map process-msg))

(defn calculate-movement-direction [key-state]
  (letfn [(adder [dx dy] (fn [[x y]] [(+ dx x) (+ dy y)]))]
    (cond-> [0 0]
      (:forward key-state) ((adder 0 1))
      (:back key-state) ((adder 0 -1))
      (:left key-state) ((adder -1 0))
      (:right key-state) ((adder 1 0))
      true (math/normalize))))

(defmulti process-tap (fn [_ type] type))

(defmethod process-tap :toggle-attack [{id :own-id :as game-state} _]
  {:new-game-state (update-in game-state [:chars id :attacking] not)
   :event {:type :toggle-attack}})

(defmethod process-tap :target [game-state _]
  (let [id (:own-id game-state)
        target (gfx/pick-target)]
    (when target
      {:new-game-state (assoc-in game-state [:chars id :target] target)
       :event {:type :target :target target}})))

(defn process-taps [game-state taps]
  (loop [[tap & more] taps game-state game-state events []]
    (if tap
      (let [{:keys [new-game-state event]} (process-tap game-state tap)]
        (recur more (or new-game-state game-state) (conj events event)))
      {:new-game-state game-state
       :events (remove nil? events)})))

(defn process-player-input [game-state key-state]
  (let [id (:own-id game-state)
        {:keys [new-game-state events]}
        (process-taps game-state (:taps key-state))
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

(defn move-self [{:keys [own-id chars move-time-delta] :as game-state}]
  (let [{:keys [pos move-dir speed] :as self} (chars own-id)
        new-pos (math/extrapolate-pos pos move-dir move-time-delta speed)]
  {:new-game-state
   (assoc-in game-state [:chars own-id :pos] new-pos)}))

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
            (reset! game-state-atom
                    (login-and-recv-state @game-state-atom net-map
                                          "leif" "star" stop?))))
        simple-update-fn
        (fn []
          (Thread/sleep 50)
          (let [{:keys [new-game-state events]}
                (ccfns/call-update-fns @game-state-atom []
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
            (reset! game-state-atom new-game-state)))
        start-fn
        (fn [this]
          (.start this))
        stop-fn
        (fn [this]
          (reset! stop? true)
          (cc/stop @graphics-system)
          (.stop this))]
    (ccfns/create-jme3-app start-fn stop-fn simple-init-fn simple-update-fn)))

(defn create-non-jme3-app [net-map game-state-atom]
  (let [stop? (atom false)]
    (reify cc/Lifecycle
      (start [this]
        (error-printing-future
          (reset! game-state-atom (login-and-recv-state
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
  (let [game-state-atom (atom (dissoc (gmap/load-game-map) :spawns))
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
