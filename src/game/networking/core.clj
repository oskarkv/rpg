(ns game.networking.core
  (:require [game.networking.implementations.kryonet :as impl]))

(def msg-types->ints (zipmap [:disconnect] (range)))

(defprotocol NetworkingConnection
  (send-reliably [this edn])
  (send-unreliably [this edn])
  (ping [this])
  (get-address [this])
  (get-connection-id [this]))

(defn construct-server [port]
  (let [queue (atom [])
        game-id-counter (atom 0)
        net-id->game-id (atom {})
        game-id->conn (atom {})
        enqueue (fn [item] (swap! queue conj item))
        get-game-id (fn [conn] (@net-id->game-id (get-connection-id conn)))
        conn-fn (fn [conn]
                  (let [game-id (swap! game-id-counter inc)
                        net-id (get-connection-id conn)]
                    (swap! net-id->game-id assoc net-id game-id)
                    (swap! game-id->conn assoc game-id conn)))
        recv-fn (fn [conn obj]
                  (let [game-id (get-game-id conn)]
                    (enqueue (conj obj game-id))))
        disc-fn (fn [conn]
                  (let [game-id (get-connection-id conn)]
                    (enqueue (list game-id (msg-types->ints :disconnect)))))
        server (impl/construct-server port conn-fn recv-fn disc-fn)
        send-fn (fn [game-id edn]
                  (send-reliably (@game-id->conn game-id) edn))]
    {:server server :queue queue :send-fn send-fn}))
