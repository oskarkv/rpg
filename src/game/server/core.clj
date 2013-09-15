(ns game.server.core
  (:require [game.networking.core :as net]
            (game.common [core :as cc]
                         [core-functions :as ccfns])
            [game.key-value-store.core :as kvs.core]
            [game.key-value-store.protocols :as kvs])
  (:use game.utils))

(defn new-player [username]
  {:name username
   :pos [0 0]
   :move-dir [0 0]})

(defmulti process-msg-purely (fn [msg _] (:type msg)))

(defmethod process-msg-purely :move [{:keys [id data] :as msg} game-state]
  (let [[pos dir] data]
    {:new-game-state
     (update-in game-state [:players id] merge {:pos pos :move-dir dir})
     :event msg}))

(defmethod process-msg-purely :default [_ game-state]
  {:new-game-state game-state})

(defmulti process-msg (fn [msg _ _] (:type msg)))

(defmethod process-msg :login [{:keys [id data]} game-state key-value-store]
  (let [[username password] data
        player (or (kvs/load key-value-store username) (new-player username))]
    {:new-game-state (assoc-in game-state [:players id] player)
     :event {:id id :type :login :data [player]}}))

(defmethod process-msg :connect [_ game-state _]
  {:new-game-state game-state})

(defmethod process-msg :disconnect [_ game-state _]
  {:new-game-state game-state})

(defmethod process-msg :default [msg game-state _]
  (process-msg-purely msg game-state))

(defmulti produce-client-msgs (fn [msg _] (:type msg)))

(defmethod produce-client-msgs :login [{id :id [player] :data} game-state]
  (let [all-players (keys (:players game-state))]
    [[[id] [:game-state game-state]]
     [all-players [:login id player]]
     [[id] [:own-id id]]]))

(defmethod produce-client-msgs :move [{id :id [pos dir] :data} game-state]
  (let [all-but-mover (keys (dissoc (:players game-state) id))]
    [[all-but-mover [:move pos dir]]]))

(defmethod produce-client-msgs :default [_ _]
  nil)

(defn extrapolate-player-movement [player]
  (let [{:keys [pos move-dir] :as player} player
        speed 1
        new-pos (map + pos (map #(* speed %) move-dir))]
    (assoc player :pos new-pos)))

(defn extrapolate-all-player-movements [{players-map :players :as game-state}]
  (let [ids (keys players-map)
        players (vals players-map)]
    (assoc game-state :players
           (zipmap ids (map extrapolate-player-movement players)))))

(defn process-network-msgs [game-state net-map key-value-store]
  (ccfns/process-network-msgs game-state net-map process-msg key-value-store))

(defn main-update [game-state {:keys [send-msg] :as net-map} key-value-store]
  (Thread/sleep 50)
  (let [{:keys [new-game-state events]}
        (process-network-msgs game-state net-map key-value-store)
        to-client-msgs (mapcat #(produce-client-msgs % new-game-state) events)]
    (doseq [[ids to-client-msg] to-client-msgs]
      (send-msg ids to-client-msg))
    new-game-state))

(defrecord Server [net-map key-value-store game-state stop?]
  cc/Lifecycle
  (start [this]
    (cc/start (:net-sys net-map))
    (cc/start key-value-store)
    (error-printing-future
      ((fn [game-state]
         (if @stop?
           nil
           (recur (main-update game-state net-map key-value-store))))
       game-state))
    this)
  (stop [this]
    (reset! stop? true)
    (cc/stop key-value-store)
    (cc/stop (:net-sys net-map))
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
                                             cc/connect-msg
                                             cc/disconnect-msg)
        new-get-msg (fn []
                      (when-let [{:keys [id msg]} (get-msg)]
                        (let [[type & data :as game-msg]
                              (cc/int->type-in-msg msg)
                              game-id (if (= cc/connect-msg game-msg)
                                        (new-game-id id)
                                        (net-id->game-id id))]
                          {:id game-id :type type :data data})))
        new-send-msg (fn [game-ids msg]
                       (let [net-msg (cc/type->int-in-msg msg)]
                         (doseq [game-id game-ids]
                           (send-msg (game-id->net-id game-id) net-msg))))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server net-map key-value-store game-state stop?)))
