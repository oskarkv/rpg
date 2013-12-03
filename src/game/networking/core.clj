(ns game.networking.core
  (:import java.util.LinkedList)
  (:require [game.networking.implementations.kryonet :as impl]
            [game.networking.protocols :as protocols]
            (game.common [core :as cc]
                         [core-functions :as ccfns])))

(defn update [net-sys]
  (cc/update net-sys nil))

(defn construct-server-net-sys [port connect-msg disconnect-msg]
  (let [queue (LinkedList.)
        id->conn (atom {})
        enqueue (fn [conn item]
                  (.add queue {:id (protocols/get-connection-id conn)
                               :msg item}))
        conn-fn (fn [conn]
                  (swap! id->conn assoc (protocols/get-connection-id conn) conn)
                  (enqueue conn (ccfns/map->msg connect-msg)))
        recv-fn (fn [conn obj]
                  (enqueue conn obj))
        disc-fn (fn [conn]
                  (enqueue conn (ccfns/map->msg disconnect-msg)))
        get-msg (fn [] (.poll queue))
        send-msg (fn [id msg]
                   (protocols/send-reliably (@id->conn id) msg))
        net-sys (:net-sys (impl/construct-server port conn-fn recv-fn disc-fn))]
    {:net-sys net-sys :get-msg get-msg :send-msg send-msg}))

(defn construct-client-net-sys [address port connect-msg disconnect-msg]
  (let [queue (LinkedList.)
        enqueue (fn [item]
                  (.add queue item))
        conn-fn (fn [conn]
                  (enqueue (ccfns/map->msg connect-msg)))
        recv-fn (fn [conn obj]
                  (enqueue obj))
        disc-fn (fn [conn]
                  (enqueue (ccfns/map->msg disconnect-msg)))
        get-msg (fn [] (.poll queue))
        {:keys [net-sys conn]} (impl/construct-client address port conn-fn
                                                      recv-fn disc-fn)
        send-msg (fn [msg]
                   (protocols/send-reliably conn msg))]
    {:net-sys net-sys :get-msg get-msg :send-msg send-msg}))
