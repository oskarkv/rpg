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

(defmulti process-msg (fn [msg _] (:type msg)))

(defmethod process-msg :login [{[id player] :data} game-state]
  (let [player (assoc player :last-move (current-time-ms))]
    {:new-game-state (assoc-in game-state [:players id] player)}))

(defmethod process-msg :own-id [{[id] :data} game-state]
  {:new-game-state (assoc game-state :own-id id)})

(defmethod process-msg :game-state [{[incoming-game-state] :data} game-state]
  {:new-game-state (merge game-state incoming-game-state)})

(defmethod process-msg :move [{[id pos] :data} game-state]
  (let [player (get-in game-state [:players id])]
    {:new-game-state
     (assoc-in game-state [:players id]
               (cond-> player
                 (nil? (:new-pos player)) (assoc :last-move (current-time-ms))
                 true (assoc :new-pos pos)))}))

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
        time-delta (/ (- curr-time last-move) 1000.0)]
    (assoc player
           :pos (math/extrapolate-pos pos move-dir time-delta speed)
           :last-move curr-time)))

(defn move-self [game-state]
  {:new-game-state
   (update-in game-state [:players (:own-id game-state)] extrapolate-player)})

(defn move-toward-new-pos [{:keys [pos new-pos last-move speed] :as player}]
  (if-not new-pos
    player
    (let [dir (math/norm-diff new-pos pos)
          curr-time (current-time-ms)
          time-delta (- curr-time last-move)
          updated-pos (math/extrapolate-pos pos dir (/ time-delta 1000.0) speed)
          new-dir (math/norm-diff new-pos updated-pos)
          dp (math/dot-product dir new-dir)]
      (if (> dp 0)
        (assoc player :pos updated-pos :last-move curr-time)
        (-> player (assoc :pos new-pos) (dissoc :new-pos :last-move))))))

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
            (:terrain @game-state-atom)))
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
                  (move-self)
                  (ccfns/move-players move-toward-new-pos))
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
  (let [game-state-atom (atom (dissoc (gmap/load-game-map) :spawns))
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
