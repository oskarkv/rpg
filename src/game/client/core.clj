(ns game.client.core
  (:import (com.jme3.system JmeContext$Type)
           (com.jme3.app FlyCamAppState))
  (:require [game.networking.core :as net]
            [game.game-map :as game-map]
            (game.client [input :as input])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [graphics :as gfx])
            [game.math :as math])
  (:use game.utils))

(defmulti process-msg (fn [msg _] (:type msg)))

(defmethod process-msg :login [{[id player] :data} game-state]
  {:new-game-state (assoc-in game-state [:players id] player)})

(defmethod process-msg :own-id [{[id] :data} game-state]
  {:new-game-state (assoc game-state :own-id id)})

(defmethod process-msg :game-state [{[incoming-game-state] :data} game-state]
  {:new-game-state (merge game-state incoming-game-state)})

(defmethod process-msg :default [_ game-state]
  {:new-game-state game-state})

(defmulti produce-server-msg (fn [_ event] (first event)))

(defmethod produce-server-msg :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:players own-id])
        pos (:pos self)
        dir (:move-dir self)]
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

(defn login-and-recv-state [game-state net-map name password]
  (let [{:keys [net-sys send-msg get-msg]} net-map]
    (send-msg [:login name password])
    (loop [game-state game-state]
      (let [new-game-state
            (:new-game-state (process-network-msgs game-state net-map))]
        (if (let [id (:own-id new-game-state)]
              (and id (get-in new-game-state [:players id])))
          new-game-state
          (recur new-game-state))))))

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

(defmacro call-update-fns [game-state events & calls]
  (let [new-events (gensym "new-events")
        new-game-state (gensym "new-game-state")]
    (if (seq calls)
      `(let [{~new-game-state :new-game-state
              ~new-events :events}
             (-> ~game-state ~(first calls))
             ~new-events (concat ~events ~new-events)]
         (call-update-fns ~new-game-state ~new-events ~@(rest calls)))
      {:new-game-state game-state :events events})))

(defn create-client-jme3-app [net-map game-state-atom]
  (let [key-bindings (input/load-key-bindings)
        key-state-atom (atom (input/create-key-state-map key-bindings))
        graphics-system (atom nil)
        init-gfx-fn
        (fn [asset-manager root-node]
          (gfx/init-graphics-system
            root-node
            asset-manager
            (:game-map @game-state-atom)))
        start-input-fn
        (fn [input-manager]
          (input/start-input
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
                    (login-and-recv-state @game-state-atom
                                          net-map
                                          "leif" "star"))))
        simple-update-fn
        (fn []
          (Thread/sleep 50)
          (let [{:keys [new-game-state events]}
                (call-update-fns @game-state-atom []
                                 (process-player-input @key-state-atom)
                                 (process-network-msgs net-map))
                to-server-msgs (->> events
                                    (map (partial produce-server-msg
                                                  new-game-state))
                                    (remove nil?))
                send-to-server (:send-msg net-map)]
            (doseq [msg to-server-msgs]
              (send-to-server msg))
            (cc/update @graphics-system new-game-state)
            (reset! game-state-atom new-game-state)))
        app (ccfns/create-jme3-app simple-init-fn simple-update-fn graphics-system)]
    app))


(defn create-non-jme3-app [net-map game-state-atom]
  (let [stop? (atom false)]
    (reify cc/Lifecycle
      (start [this]
        (error-printing-future
          (reset! game-state-atom (login-and-recv-state
                                    @game-state-atom net-map "leif" "star"))
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
