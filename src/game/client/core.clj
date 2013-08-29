(ns game.client.core
  (:import (com.jme3.app SimpleApplication
                         FlyCamAppState)
           (com.jme3.system AppSettings
                            JmeContext$Type)
           (com.jme3.input KeyInput
                           MouseInput)
           (com.jme3.input.controls KeyTrigger
                                    ActionListener))
  (:require [game.networking.core :as net]
            [game.game-map :as game-map]
            [game.client.graphics :as gfx]
            [game.core :as core])
  (:use game.utils))

(defmulti process-msg (fn [msg _] (:type msg)))

(defmethod process-msg :login [{[id player] :data} game-state]
  (assoc-in game-state [:players id] player))

(defmethod process-msg :game-state [{[incoming-game-state] :data} game-state]
  (merge game-state incoming-game-state))

(defmethod process-msg :default [_ game-state]
  game-state)

(defn main-update [{:keys [net-sys get-msg send-msg]} game-state key-state]
  (Thread/sleep 50)
  (net/update net-sys)
  (if (:forward key-state) (println "forward"))
  (loop [msg (get-msg) game-state game-state]
    (if msg
      (let [new-game-state (process-msg msg game-state)]
        (net/update net-sys)
        (if-let [msg (get-msg)]
          (recur msg new-game-state)
          new-game-state))
      game-state)))

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

(def key-bindings [["forward" "w" :hold]])

(defn string->KeyInput [s]
  (-> (.getField KeyInput (str "KEY_" (.toUpperCase s))) (.get nil)))

(defn create-KeyInputs [key-bindings]
  (map (fn [[name key type]] [name (string->KeyInput key) type])
       key-bindings))

(defn create-key-state-map [key-bindings]
  (zipmap (map (comp keyword first)
               (filter (fn [[_ _ type]] (= type :hold)) key-bindings))
          (repeat false)))

(defn init-input [input-manager key-bindings key-state-atom]
  (let [keyinput-bindings (create-KeyInputs key-bindings)
        hold-listener (reify ActionListener
                        (onAction [this name pressed tpf]
                          (swap! key-state-atom assoc (keyword name) pressed)))]
    (doseq [[name key _] keyinput-bindings]
      (.addMapping input-manager name (into-array [(KeyTrigger. key)])))
    (.addListener input-manager
                  hold-listener
                  (into-array (map first key-bindings)))))

(defn create-jme3-app [net-map game-state-atom key-state-atom]
  (let [app
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
                    (init-input input-manager key-bindings key-state-atom)
                    #_(gfx/init-graphics root-node asset-manager game-map)))
                (simpleUpdate [tpf]
                  (reset! game-state-atom
                          (main-update net-map @game-state-atom
                                       @key-state-atom))))
          (.setShowSettings false)
          (.setSettings (AppSettings. true))
          (.setPauseOnLostFocus false))]
    (extend-type (type app)
      core/Lifecycle
      (start [this]
        (.start this))
      (stop [this]
        (.stop this)))
    app))

(defn create-non-jme3-app [net-map game-state-atom key-state-atom]
  (let [stop? (atom false)]
    (reify core/Lifecycle
      (start [this]
        (error-printing-future
          ((fn []
            (reset! game-state-atom
                    (main-update net-map @game-state-atom
                                 @key-state-atom))
            (if @stop? nil (recur))))))
      (stop [this]
        (reset! stop? true)))))

(defn init-client [address port headless]
  (let [game-state-atom (atom {:game-map (game-map/load-game-map)})
        key-state-atom (atom (create-key-state-map key-bindings))
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
              (create-non-jme3-app net-map game-state-atom key-state-atom)
              (create-jme3-app net-map game-state-atom key-state-atom))]
    (->Client net-map app)))
