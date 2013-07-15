(ns game.networking.core
  (:import java.util.LinkedList)
  (:require [game.networking.implementations.kryonet :as impl]
            [game.networking.protocols :as protocols]
            [game.core :as core]))

(defn construct-server [port connect-msg disconnect-msg]
  (let [queue (LinkedList.)
        id->conn (atom {})
        enqueue (fn [conn item]
                  (.add queue {:id (protocols/get-connection-id conn)
                               :msg item}))
        conn-fn (fn [conn]
                  (swap! id->conn assoc (protocols/get-connection-id conn) conn)
                  (enqueue conn (core/type->int connect-msg)))
        recv-fn (fn [conn obj]
                  (enqueue conn obj))
        disc-fn (fn [conn]
                  (enqueue conn (core/type->int disconnect-msg)))
        net-sys (impl/construct-server port conn-fn recv-fn disc-fn)
        get-msg (fn [] (.poll queue))
        send-msg (fn [id msg]
                  (protocols/send-reliably (@id->conn id) msg))]
    {:net-sys net-sys :get-msg get-msg :send-msg send-msg}))
