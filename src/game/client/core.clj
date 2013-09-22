(ns game.client.core
  (:import (com.jme3.system JmeContext$Type)
           (com.jme3.app FlyCamAppState))
  (:require [game.networking.core :as net]
            [game.game-map :as game-map]
            (game.client [input :as c-input])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [input :as cmn-input]
                         [graphics :as gfx])
            [game.math :as math])
  (:use game.utils))

(defmulti process-msg (fn [msg _] (:type msg)))

(defmethod process-msg :login [{[id player] :data} game-state]
  (let [player (assoc player :last-move (current-time-ms))]
    {:new-game-state (assoc-in game-state [:players id] player)}))

(defmethod process-msg :own-id [{[id] :data} game-state]
  {:new-game-state (assoc game-state :own-id id)})

(defmethod process-msg :game-state [{[incoming-game-state] :data} game-state]
  {:new-game-state (merge game-state incoming-game-state)})

(defmethod process-msg :move [{[id pos move-dir] :data} game-state]
  (let [last-dir (get-in game-state [:players id :move-dir])]
    {:new-game-state
     (assoc-in game-state [:players id]
               (-> game-state :players (get id)
                   (cond->
                     (= [0 0] last-dir) (assoc :last-move (current-time-ms)))
                   (assoc :new-pos pos :move-dir move-dir)
                   (dissoc :interp-time-left :interp-speed :old-pos)))}))

(defmethod process-msg :default [_ game-state]
  {:new-game-state game-state})

(defmulti produce-server-msg (fn [_ event] (first event)))

(defmethod produce-server-msg :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:players own-id])
        pos (map float (:pos self))
        dir (map float (:move-dir self))]
    [:move pos dir]))

(defmethod produce-server-msg :default [game-state event]
  nil)

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

(defn process-player-input [game-state key-state]
  (let [id (:own-id game-state)
        old-dir (map float (get-in game-state [:players id :move-dir]))
        new-dir (map float (calculate-movement-direction key-state))
        new-game-state (assoc-in game-state [:players id :move-dir] new-dir)
        return-map {:new-game-state new-game-state}]
    (if-not (= old-dir new-dir)
      (conj return-map {:events [[:new-dir]]})
      return-map)))

(defn process-received-game-state [{:keys [players] :as game-state}]
  (let [curr-time (current-time-ms)]
    (assoc game-state :players
           (fmap (fn [player] (assoc player :last-move curr-time)) players))))

(defn login-and-recv-state [game-state net-map name password stop?]
  (let [{:keys [net-sys send-msg get-msg]} net-map]
    (send-msg [:login name password])
    (loop [game-state game-state]
      (let [new-game-state
            (:new-game-state (process-network-msgs game-state net-map))]
        (if (let [id (:own-id new-game-state)]
              (or @stop? (and id (get-in new-game-state [:players id]))))
          (process-received-game-state new-game-state)
          (recur new-game-state))))))

(defn extrapolate-player [{:keys [pos move-dir last-move speed] :as player}]
  (let [curr-time (current-time-ms)
        time-delta (/ (- curr-time last-move) 1000)]
    (assoc player
           :pos (math/extrapolate-pos pos move-dir time-delta speed)
           :last-move curr-time)))

(defn set-up-interpolation [{:keys [pos new-pos move-dir speed] :as player}]
  (let [interp-time 0.1
        new-new-pos (math/extrapolate-pos new-pos move-dir interp-time speed)
        interp-speed (/ (math/distance new-new-pos pos) interp-time)]
    (assoc player :new-pos new-new-pos
           :interp-time-left (* 1000 interp-time)
           :interp-speed interp-speed)))

(defn interpolate-player
  [{:keys [new-pos interp-time-left interp-speed last-move pos] :as player}]
  (let [curr-time (current-time-ms)
        time-delta (- curr-time last-move)
        interp-time (min time-delta interp-time-left)
        dir (math/normalize (map - new-pos pos))
        updated-pos (math/extrapolate-pos pos dir (/ interp-time 1000)
                                          interp-speed)
        time-left? (>= time-delta interp-time-left)]
    (cond-> player
      :always (assoc :pos updated-pos :last-move (+ last-move interp-time)
                     :interp-time-left (- interp-time-left interp-time))
      time-left? (-> (dissoc player :interp-speed :interp-time-left :new-pos)
                     extrapolate-player))))

(defn move-player [{:keys [interp-time-left new-pos] :as player}]
  (cond interp-time-left (interpolate-player player)
        new-pos (interpolate-player (set-up-interpolation player))
        :else (extrapolate-player player)))

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
        (fn [asset-manager root-node]
          (gfx/init-graphics-system
            root-node
            asset-manager
            (:game-map @game-state-atom)))
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
            (reset! graphics-system
                    (init-gfx-fn
                      asset-manager root-node))
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
                  (ccfns/move-players move-player))
                to-server-msgs (->> events
                                    (map (partial produce-server-msg
                                                  new-game-state))
                                    (remove nil?))
                send-to-server (:send-msg net-map)]
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
  (let [game-map (game-map/load-game-map)
        game-state-atom (atom {:game-map game-map})
        {:keys [net-sys get-msg send-msg]}
        (net/construct-client-net-sys address port
                                      cc/connect-msg
                                      cc/disconnect-msg)
        new-get-msg (fn []
                      (when-let [msg (get-msg)]
                        (let [[type & data :as game-msg]
                              (cc/int->type-in-msg msg)]
                          {:type type :data data})))
        new-send-msg (fn [msg]
                       (send-msg (cc/type->int-in-msg msg)))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        app (if headless
              (create-non-jme3-app net-map game-state-atom)
              (create-client-jme3-app net-map game-state-atom))]
    (->Client net-map app)))
