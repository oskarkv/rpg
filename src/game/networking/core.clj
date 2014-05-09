(ns game.networking.core
  (:import java.util.LinkedList)
  (:require [game.networking.implementations.kryonet :as impl]
            [game.networking.protocols :as protocols]
            (game.common [core :as cc]
                         [core-functions :as ccfns])))

(deftype NetworkingSystem [send-msg-fn msg-queue-ref impl-sys msg-types-set]
  cc/Lifecycle
  (start [this]
    (cc/start impl-sys))
  (stop [this]
    (cc/stop impl-sys))
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue msg-queue-ref))
  cc/Updatable
  (update [this events]
    (->> events
         (filter (fn [msg] (contains? msg-types-set (:type msg))))
         (map send-msg-fn)
         dorun)))

(deftype ServerNetworkingSystem [send-msg-fn msg-queue-ref impl-sys]
  cc/Lifecycle
  (start [this]
    (cc/start impl-sys))
  (stop [this]
    (cc/stop impl-sys))
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue msg-queue-ref))
  cc/Updatable
  (update [this msgs]
    (doseq [[ids msg] msgs]
      (doseq [id ids]
        (send-msg-fn id msg)))))

(defn init-server-net-sys [port net-id->game-id game-id->net-id]
  (let [msg-queue-ref (ref [])
        id->conn (atom {})
        enqueue (fn [conn game-msg]
                  (ccfns/queue-conj
                    msg-queue-ref
                    (assoc game-msg
                           :id (net-id->game-id
                                 (protocols/get-connection-id conn)))))
        conn-fn (fn [conn]
                  (swap! id->conn assoc (protocols/get-connection-id conn) conn)
                  (enqueue conn cc/connect-msg))
        recv-fn (fn [conn obj]
                  (enqueue conn (ccfns/msg->map obj)))
        disc-fn (fn [conn]
                  (enqueue conn cc/disconnect-msg))
        send-msg (fn [id msg-map]
                   (protocols/send-reliably (@id->conn (game-id->net-id id))
                                            (ccfns/map->msg msg-map)))
        net-sys (:net-sys (impl/construct-server port conn-fn recv-fn disc-fn))]
    (->ServerNetworkingSystem send-msg msg-queue-ref net-sys)))

(defn init-client-net-sys [address port]
  (let [msg-queue-ref (ref [])
        enqueue (fn [item]
                  (ccfns/queue-conj msg-queue-ref item))
        recv-fn (fn [conn obj]
                  (enqueue (ccfns/msg->map obj)))
        conn-fn (fn [conn]
                  (enqueue cc/connect-msg))
        disc-fn (fn [conn]
                  (enqueue cc/disconnect-msg))
        {:keys [net-sys conn]} (impl/construct-client address port conn-fn
                                                      recv-fn disc-fn)
        send-msg (fn [msg-map]
                   (protocols/send-reliably conn (ccfns/map->msg msg-map)))
        msg-types-set (set (filter (fn [k] (.startsWith (str k) ":c-"))
                                   (keys cc/type->keys)))]
    (->NetworkingSystem send-msg msg-queue-ref net-sys msg-types-set)))
