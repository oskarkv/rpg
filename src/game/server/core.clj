(ns game.server.core
  (:require [game.networking.core :as net]
            (game.common [core :as cc]
                         [core-functions :as ccfns])
            (game.key-value-store [core :as kvs.core]
                                  [protocols :as kvs])
            [game.math :as math])
  (:use game.utils))

(defn new-player [username]
  {:name username
   :speed 1
   :pos [0 0]
   :old-recv-pos [0 0]
   :move-dir [0 0]})

(defmulti process-msg-purely (fn [msg _] (:type msg)))

(defmethod process-msg-purely :move [{:keys [id data] :as msg} game-state]
  (let [[pos dir] data]
    {:new-game-state
     (-> game-state
         (update-in [:players id] merge
                    {:recv-pos pos :move-dir dir :recv-time (current-time-ms)})
         (update-in [:players id :recv-this-frame] conj pos))
     :event msg}))

(defmethod process-msg-purely :default [_ game-state]
  {:new-game-state game-state})

(defmulti process-msg (fn [msg _ _] (:type msg)))

(defmethod process-msg :login [{:keys [id data]} game-state key-value-store]
  (let [[username password] data
        curr-time (current-time-ms)
        player (assoc (or (kvs/load key-value-store username)
                          (new-player username))
                      :old-recv-time curr-time :last-move curr-time)]
    {:new-game-state (assoc-in game-state [:players id] player)
     :event {:id id :type :login :data [player]}}))

(defmethod process-msg :connect [_ game-state _]
  {:new-game-state game-state})

(defmethod process-msg :disconnect [_ game-state _]
  {:new-game-state game-state})

(defmethod process-msg :default [msg game-state _]
  (process-msg-purely msg game-state))

(defmulti produce-client-msgs (fn [msg _] (:type msg)))

(defn prepare-players-for-sending [players]
  (fmap (fn [player] (select-keys player [:speed :name :move-dir :pos]))
        players))

(defmethod produce-client-msgs :login [{id :id} game-state]
  (let [all-players (keys (:players game-state))
        game-state-to-send (update-in game-state [:players]
                                      prepare-players-for-sending)]
    [[[id] [:game-state game-state-to-send]]
     [all-players [:login id (get-in game-state-to-send [:players id])]]
     [[id] [:own-id id]]]))

(defmethod produce-client-msgs :move [{id :id [pos dir] :data} game-state]
  (let [all-but-mover (keys (dissoc (:players game-state) id))]
    [[all-but-mover [:move id pos dir]]]))

(defmethod produce-client-msgs :default [_ _]
  nil)

(defn move-from-recv-pos
  [{:keys [recv-time recv-pos move-dir speed] :as player}]
  ; TOTO: Use old-recv-pos, recv-this-frame, recv-time and old-recv-time to
  ; calculate the distance and time the player has moved this frame, and save
  ; that somewhere so that the server can check that the player is not moving
  ; too fast.
  (let [curr-time (current-time-ms)
        extrap-time (- curr-time recv-time)
        new-pos (math/extrapolate-pos recv-pos move-dir
                                      (/ extrap-time 1000) speed)]
    (-> player
        (assoc :pos new-pos :old-recv-pos recv-pos
               :last-move curr-time :old-recv-time recv-time)
        (dissoc :recv-this-frame))))

(defn move-player
  [{:keys [pos last-move move-dir recv-this-frame speed] :as player}]
  (if recv-this-frame
    (move-from-recv-pos player)
    (let [curr-time (current-time-ms)
          time-delta (/ (- curr-time last-move) 1000)]
      (assoc player :pos (math/extrapolate-pos pos move-dir time-delta speed)
             :last-move curr-time))))

(defn process-network-msgs [game-state net-map key-value-store]
  (ccfns/process-network-msgs game-state net-map process-msg key-value-store))

(defn main-update [game-state {:keys [send-msg] :as net-map} key-value-store]
  (Thread/sleep 50)
  (let [{:keys [new-game-state events]}
        (ccfns/call-update-fns game-state []
          (process-network-msgs net-map key-value-store)
          (ccfns/move-players move-player))
        to-client-msgs (mapcat #(produce-client-msgs % new-game-state) events)]
    (doseq [[ids to-client-msg] to-client-msgs]
      (send-msg ids to-client-msg))
    new-game-state))

(defrecord Server [net-map key-value-store game-state stop?]
  cc/Lifecycle
  (start [this]
    (cc/start (:net-sys net-map))
    (cc/start key-value-store)
    (start-new-thread "server"
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
