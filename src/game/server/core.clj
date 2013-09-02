(ns game.server.core
  (:require [game.networking.core :as net]
            [game.core :as core]
            [game.key-value-store.core :as kvs.core]
            [game.key-value-store.protocols :as kvs])
  (:use game.utils))

(defn new-player [username]
  {:name username
   :pos [0 0]
   :move-dir [0 0]})

(defmulti process-msg-purely (fn [msg _] (:type msg)))

(defmethod process-msg-purely :default [_ game-state]
  game-state)

(defmulti process-msg (fn [msg _ _] (:type msg)))

(defmethod process-msg :login [{:keys [id data]} game-state key-value-store]
  (let [[username password] data
        player (or (kvs/load key-value-store username) (new-player username))]
    {:new-game-state (assoc-in game-state [:players id] player)
     :client-delta {:id id :type :login :data [player]}}))

(defmethod process-msg :connect [_ game-state _]
  {:new-game-state game-state})

(defmethod process-msg :disconnect [_ game-state _]
  {:new-game-state game-state})

(defmethod process-msg :default [msg game-state _]
  (process-msg-purely msg game-state))

(defmulti prepare-client-msgs (fn [msg _] (:type msg)))

(defmethod prepare-client-msgs :login [{id :id [player] :data} game-state]
  (let [all-players (keys (:players game-state))]
    [[[id] [:game-state game-state]]
     [all-players [:login id player]]
     [[id] [:own-id id]]]))

(defmethod prepare-client-msgs :default [_ _]
  nil)

(defn main-loop [{:keys [net-sys get-msg send-msg]}
                 key-value-store game-state stop?]
  (loop [game-state game-state]
    (Thread/sleep 50)
    (net/update net-sys)
    (let [new-game-state
          (loop [msg (get-msg) game-state game-state]
            (if msg
              (let [{:keys [new-game-state client-delta]}
                    (process-msg msg game-state key-value-store)
                    to-client-msgs (prepare-client-msgs client-delta
                                                        new-game-state)]
                (doseq [[ids to-client-msg] to-client-msgs]
                  (send-msg ids to-client-msg))
                (net/update net-sys)
                (if-let [msg (get-msg)]
                  (recur msg new-game-state)
                  new-game-state))
              game-state))]
      (if-not @stop? (recur new-game-state)))))

(defrecord Server [net-map key-value-store game-state stop?]
  core/Lifecycle
  (start [this]
    (core/start (:net-sys net-map))
    (core/start key-value-store)
    (error-printing-future (main-loop net-map key-value-store game-state stop?))
    this)
  (stop [this]
    (reset! stop? true)
    (core/stop key-value-store)
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
  (let [game-state {:players {}}
        stop? (atom false)
        {:keys [net-sys get-msg send-msg]} (net/construct-server-net-sys
                                             port
                                             core/connect-msg
                                             core/disconnect-msg)
        new-get-msg (fn []
                      (when-let [{:keys [id msg]} (get-msg)]
                        (let [[type & data :as game-msg]
                              (core/int->type-in-msg msg)
                              game-id (if (= core/connect-msg game-msg)
                                        (new-game-id id)
                                        (net-id->game-id id))]
                          {:id game-id :type type :data data})))
        new-send-msg (fn [game-ids msg]
                       (let [net-msg (core/type->int-in-msg msg)]
                         (doseq [game-id game-ids]
                           (send-msg (game-id->net-id game-id) net-msg))))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server net-map key-value-store game-state stop?)))
