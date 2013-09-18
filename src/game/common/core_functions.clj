(ns game.common.core-functions
  (:import (com.jme3.system AppSettings)
           (com.jme3.app SimpleApplication))
  (:require [game.common.core :as cc]))

(defn process-network-msgs [game-state {:keys [net-sys get-msg send-msg]}
                            process-fn & process-args]
  (cc/update net-sys nil)
  (loop [msg (get-msg) game-state game-state events []]
    (if msg
      (let [{:keys [new-game-state event]}
            (apply process-fn msg game-state process-args)]
        (cc/update net-sys nil)
        (recur (get-msg) new-game-state (conj events event)))
      {:new-game-state game-state :events (remove nil? events)})))

(defn create-jme3-app [start-fn stop-fn init-fn update-fn]
  (let [app
        (doto (proxy [SimpleApplication] []
                (simpleInitApp []
                  (init-fn this))
                (simpleUpdate [tpf]
                  (update-fn)))
          (.setShowSettings false)
          (.setSettings (AppSettings. true))
          (.setPauseOnLostFocus false))]
    (extend-type (type app)
      cc/Lifecycle
      (start [this]
        (start-fn this))
      (stop [this]
        (stop-fn this)))
    app))
