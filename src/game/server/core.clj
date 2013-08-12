(ns game.server.core
  (:require [game.networking.core :as net]
            [game.core :as core]
            [game.key-value-store.core :as kvs.core]
            [game.key-value-store.protocols :as kvs])
  (:use game.utils))

(defn new-player [username]
  {:name username
   :pos [0 0]})

(defmulti process-delta (fn [delta _] (:type delta)))

(defmethod process-delta :new-player [delta game-state]
  (let [{:keys [id player]} delta]
    (assoc-in game-state [:players id] player)))


(defmulti produce-delta-from-msg :type)

(defmethod produce-delta-from-msg :default [_])


(defmulti process-msg (fn [msg _ _] (:type msg)))

(defmethod process-msg :login [{:keys [id data]} game-state key-value-store]
  (println "process-msg" id data)
  (let [[username] data
        player (or (kvs/load key-value-store username) (new-player username))]
    {:type :new-player :id id :player player}))

(defmethod process-msg :connect [_ _ _])

(defmethod process-msg :disconnect [_ _ _])

(defmethod process-msg :default [msg game-state _]
  (produce-delta-from-msg msg game-state))

(defn main-loop [net-map key-value-store game-state stop?]
  (loop [game-state game-state]
    (println game-state)
    (Thread/sleep 100)
    (core/update (:net-sys net-map))
    (let [{:keys [net-sys get-msg send-msg]} net-map
          new-game-state
          (loop [msg (get-msg) game-state game-state]
            (if msg
              (let [delta (process-msg msg game-state key-value-store)
                    new-game-state (if (nil? delta)
                                     game-state
                                     (process-delta delta game-state))]
                (core/update net-sys)
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
    (core/stop (:net-sys net-map))
    (core/stop key-value-store)
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
        {:keys [net-sys get-msg send-msg]} (net/construct-net-sys
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
        new-send-msg (fn [game-id msg]
                       (send-msg (game-id->net-id game-id)
                                 (core/type->int-in-msg msg)))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server net-map key-value-store game-state stop?)))
