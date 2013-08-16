(ns game.client.core
  (:require [game.networking.core :as net]
            [game.core :as core])
  (:use game.utils))

(defmulti process-msg (fn [msg _] (:type msg)))

(defmethod process-msg :login [{[id player] :data} game-state]
  (assoc-in game-state [:players id] player))

(defmethod process-msg :default [_ game-state]
  game-state)

(defn main-loop [{:keys [net-sys get-msg send-msg]} game-state stop?]
  (loop [game-state game-state]
  (println "--CLIENT--" game-state)
    (Thread/sleep 50)
    (core/update net-sys)
    (let [new-game-state
          (loop [msg (get-msg) game-state game-state]
            (if msg
              (let [new-game-state (process-msg msg game-state)]
                (core/update net-sys)
                (if-let [msg (get-msg)]
                  (recur msg new-game-state)
                  new-game-state))
              game-state))]
      (if-not @stop? (recur new-game-state)))))

(defrecord Client [net-map game-state stop?]
  core/Lifecycle
  (start [this]
    (core/start (:net-sys net-map))
    (error-printing-future (main-loop net-map game-state stop?))
    this)
  (stop [this]
    (reset! stop? true)
    (core/stop (:net-sys net-map))
    this))

(defn init-client [address port]
  (let [game-state {:players {}}
        stop? (atom false)
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
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}]
    (->Client net-map game-state stop?)))
