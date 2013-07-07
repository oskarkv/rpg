(ns game.networking.implementations.kryonet
  (:import (com.esotericsoftware.kryonet Server Client Listener Connection)
           (java.net InetAddress))
  (:require [game.networking.core :as net]))

(defn from-edn [edn]
  (pr-str edn))

(defn to-edn [obj]
  (read-string obj))

(extend-type Connection
  net/NetworkingConnection
  (send-reliably [this edn]
    (.sendTCP this (from-edn edn)))
  (send-unreliably [this edn]
    (.sendTCP this (from-edn edn)))
  (ping [this]
    (.getReturnTripTime this))
  (get-address [this]
    (.getHostString (.getRemoteAddressTCP this))))


(defmacro defnetworkingsystem [name args & start-body]
  `(deftype ~name ~args
     net/NetworkingSystem
     (~'start [~'this]
       ~@start-body)
     (~'stop [~'this]
       (.stop ~(first args)))
     (~'update [~'this]
       ; Maybe having a positive timeout is better.
       (.update ~(first args) 0))))

(defnetworkingsystem KryonetServer [server port]
  ; .bind must be called in a thread other than the update thread, but
  ; update needs to be called at the same time; blocks until bound.
  (def sf (future (.bind server port))))

(defnetworkingsystem KryonetClient [client address port]
  ; .connect must be called in a thread other than the update thread, but
  ; update needs to be called at the same time; blocks until connected.
  (def cf (future (.connect client 3000 (InetAddress/getByName address) port))))

(defn construct-system [base-object creation-fn args conn-fn recv-fn disc-fn]
    (let [listener (proxy [Listener] []
                   (connected [conn]
                     (conn-fn conn))
                   (received [conn obj]
                     (recv-fn conn (to-edn obj)))
                   (disconnected [conn]
                     (disc-fn conn)))
        system (apply creation-fn base-object args)]
    (.addListener base-object listener)
    system))

(defn construct-server [port conn-fn recv-fn disc-fn]
  (construct-system (Server.) ->KryonetServer [port] conn-fn recv-fn disc-fn))

(defn construct-client [address port conn-fn recv-fn disc-fn]
  (construct-system (Client.) ->KryonetClient [address port] conn-fn recv-fn disc-fn))
