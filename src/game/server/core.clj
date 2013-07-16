(ns game.server.core
  (:require [game.networking.core :as net]
            [game.core :as core])
  (:use game.utils))

(defn process-msg [msg]
  nil)

(defn main-loop [net-map game-state stop?]
  (Thread/sleep 10)
  (let [{:keys [net-sys get-msg send-msg]} net-map]
    (core/update net-sys)
    (while-let [msg (get-msg)]
      (process-msg msg)
      (core/update net-sys))
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

(let [game-id-counter (atom 0)
      net->game (atom {})
      game->net (atom {})]
  (defn new-game-id [net-id]
    (let [game-id (swap! game-id-counter inc)]
      (swap! net->game assoc net-id game-id)
      (swap! game->net assoc game-id net-id)
      game-id))
  (defn net-id->game-id [net-id]
    (@net->game net-id))
  (defn game-id->net-id [game-id]
    (@game->net game-id)))

(defn init-server [port]
  (let [game-state {}
        stop? (atom false)
        {:keys [net-sys get-msg send-msg]} (net/construct-net-sys
                                             port
                                             core/connect-msg
                                             core/disconnect-msg)
        new-get-msg (fn []
                      (when-let [{:keys [id msg]} (get-msg)]
                        (let [game-id (if (= core/connect-msg msg)
                                        (new-game-id id)
                                        (net-id->game-id id))
                              msg (core/int->type msg)]
                          {:id game-id :msg msg})))
        new-send-msg (fn [game-id msg]
                       (send-msg (game-id->net-id game-id)
                                 (core/type->int msg)))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}]
    (->Server net-map game-state stop?)))
