(ns game.client.core
  (:import (com.jme3.app SimpleApplication
                         FlyCamAppState)
           (com.jme3.system AppSettings
                            JmeContext$Type))
  (:require [game.networking.core :as net]
            [game.game-map :as game-map]
            (game.client [graphics :as gfx]
                         [input :as input])
            [game.core :as core])
  (:use game.utils))

(defmulti process-msg (fn [msg _] (:type msg)))

(defmethod process-msg :login [{[id player] :data} game-state]
  (assoc-in game-state [:players id] player))

(defmethod process-msg :game-state [{[incoming-game-state] :data} game-state]
  (merge game-state incoming-game-state))

(defmethod process-msg :default [_ game-state]
  game-state)

(defn process-network-msgs [{:keys [net-sys get-msg send-msg]} game-state]
  (Thread/sleep 50)
  (net/update net-sys)
  (loop [msg (get-msg) game-state game-state]
    (if msg
      (let [new-game-state (process-msg msg game-state)]
        (net/update net-sys)
        (if-let [msg (get-msg)]
          (recur msg new-game-state)
          new-game-state))
      game-state)))

(defn process-player-input [key-state]
  (if (:forward key-state) (println "forward")))

(defrecord Client [net-map app]
  core/Lifecycle
  (start [this]
    (core/start (:net-sys net-map))
    (core/start app)
    this)
  (stop [this]
    (core/stop app )
    (core/stop (:net-sys net-map))
    this))

(defn create-jme3-app [net-map game-state-atom]
  (let [key-bindings (input/load-key-bindings)
        key-state-atom (atom (input/create-key-state-map key-bindings))
        graphics-system (atom nil)
        app
        (doto (proxy [SimpleApplication] []
                (simpleInitApp []
                  (let [{:keys [game-map]} @game-state-atom
                        input-manager (.getInputManager this)
                        state-manager (.getStateManager this)
                        asset-manager (.getAssetManager this)
                        root-node (.getRootNode this)]
                    (.detach state-manager (.getState state-manager
                                                      FlyCamAppState))
                    (.setCursorVisible input-manager true)
                    (input/start-input
                      input-manager key-bindings key-state-atom)
                    (reset! graphics-system
                            (gfx/init-graphics-system
                              root-node asset-manager game-map))
                    (core/start @graphics-system)))
                (simpleUpdate [tpf]
                  (process-player-input @key-state-atom)
                  (reset! game-state-atom
                          (process-network-msgs net-map @game-state-atom))
                  (core/update @graphics-system @game-state-atom)))
          (.setShowSettings false)
          (.setSettings (AppSettings. true))
          (.setPauseOnLostFocus false))]
    (extend-type (type app)
      core/Lifecycle
      (start [this]
        (.start this))
      (stop [this]
        (core/stop @graphics-system)
        (.stop this)))
    app))

(defn create-non-jme3-app [net-map game-state-atom]
  (let [stop? (atom false)]
    (reify core/Lifecycle
      (start [this]
        (error-printing-future
          ((fn []
            (reset! game-state-atom
                    (process-network-msgs net-map @game-state-atom))
            (if @stop? nil (recur))))))
      (stop [this]
        (reset! stop? true)))))

(defn init-client [address port headless]
  (let [game-map (game-map/load-game-map)
        game-state-atom (atom {:game-map game-map})
        {:keys [net-sys get-msg send-msg]}
        (net/construct-client-net-sys address port
                                      core/connect-msg
                                      core/disconnect-msg)
        new-get-msg (fn []
                      (when-let [msg (get-msg)]
                        (let [[type & data :as game-msg]
                              (core/int->type-in-msg msg)]
                          {:type type :data data})))
        new-send-msg (fn [msg]
                       (send-msg (core/type->int-in-msg msg)))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        app (if headless
              (create-non-jme3-app net-map game-state-atom)
              (create-jme3-app net-map game-state-atom))]
    (->Client net-map app)))
