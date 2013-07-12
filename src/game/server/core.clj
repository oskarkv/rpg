(ns game.server.core
  (:require [game.networking.core :as net]
            [game.core :as core])
  (:use game.utils))

(def ^:const port 12345)

(defn process-msgs [msgs]
  nil)

(defn main-loop [net-map game-state stop?]
  (Thread/sleep 1)
  (let [{:keys [net-sys queue send-fn]} net-map]
    (loop [msgs @queue]
      (process-msgs msgs)
      (reset! queue [])
      (core/update net-sys)
      (if-let [queue (seq @queue)]
        (recur queue)))
    (if-not @stop? (recur net-map game-state stop?))))

(defrecord Server [net-map game-state stop?]
  core/Lifecycle
  (start [this]
    (let [{net-sys :net-sys} net-map]
      (core/start net-sys)
      (error-printing-future (main-loop net-map game-state stop?))
      this))
  (stop [this]
    (reset! stop? true)
    (core/stop (:net-sys net-map))
    this))

(defn init-server [port]
  (let [game-state {}
        stop? (atom false)
        net-map (net/construct-server port)]
    (->Server net-map game-state stop?)))
