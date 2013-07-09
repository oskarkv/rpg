(ns game.networking.core
  (:require [game.networking.implementations.kryonet :as impl]
            [game.networking.protocols :as protocols]))

(def msg-types->ints (zipmap [:disconnect] (range)))

(defn construct-server [port]
  (let [queue (atom [])
        game-id-counter (atom 0)
        net-id->game-id (atom {})
        game-id->conn (atom {})
        enqueue (fn [item] (swap! queue conj item))
        get-game-id (fn [conn] (@net-id->game-id (protocols/get-connection-id conn)))
        conn-fn (fn [conn]
                  (let [game-id (swap! game-id-counter inc)
                        net-id (protocols/get-connection-id conn)]
                    (swap! net-id->game-id assoc net-id game-id)
                    (swap! game-id->conn assoc game-id conn)))
        recv-fn (fn [conn obj]
                  (let [game-id (get-game-id conn)]
                    (enqueue (conj obj game-id))))
        disc-fn (fn [conn]
                  (let [game-id (protocols/get-connection-id conn)]
                    (enqueue (list game-id (msg-types->ints :disconnect)))))
        net-sys (impl/construct-server port conn-fn recv-fn disc-fn)
        send-fn (fn [game-id edn]
                  (protocols/send-reliably (@game-id->conn game-id) edn))]
    {:net-sys net-sys :queue queue :send-fn send-fn}))
