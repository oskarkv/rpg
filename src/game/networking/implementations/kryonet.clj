(ns game.networking.implementations.kryonet
  (:import (com.esotericsoftware.kryonet Server Client Listener Connection)
           (java.net InetAddress))
  (:require [game.networking.protocols :as net]
            [game.common :as cmn])
  (:use game.utils))

(defn- from-edn [edn]
  (pr-str edn))

(defn- to-edn [obj]
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
    (.getHostString (.getRemoteAddressTCP this)))
  (get-connection-id [this]
    (.getID this)))


(defmacro- defnetworkingsystem- [name args & start-body]
  `(deftype- ~name ~args
     cmn/Lifecycle
     (~'start [~'this]
       ~@start-body)
     (~'stop [~'this]
       (.stop ~(first args)))
     cmn/Updatable
     (~'update [~'this ~'_]
       ; Maybe having a positive timeout is better.
       (.update ~(first args) 0))))

(defnetworkingsystem- KryonetServer [server port]
  ; .bind must be called in a thread other than the update thread, but
  ; update needs to be called at the same time; blocks until bound.
  (error-printing-future (.bind server port))
  (.update server 0))

(defnetworkingsystem- KryonetClient [client address port]
  ; .connect must be called in a thread other than the update thread, but
  ; update needs to be called at the same time; blocks until connected.
  (error-printing-future
    (.connect client 5000 (InetAddress/getByName address) port))
  (.update client 0))

(defn- construct-system [base-object creation-fn args conn-fn recv-fn disc-fn]
    (let [listener (proxy [Listener] []
                   (connected [conn]
                     (conn-fn conn))
                   (received [conn obj]
                     (if (string? obj)
                       (recv-fn conn (to-edn obj))))
                   (disconnected [conn]
                     (disc-fn conn)))
        system (apply creation-fn base-object args)]
    (.addListener base-object listener)
    system))

(defn construct-server [port conn-fn recv-fn disc-fn]
  {:net-sys (construct-system (Server.) ->KryonetServer
                              [port] conn-fn recv-fn disc-fn)})

(defn construct-client [address port conn-fn recv-fn disc-fn]
  (let [client (Client.)]
    {:net-sys (construct-system client ->KryonetClient
                                [address port] conn-fn recv-fn disc-fn)
     :conn client}))
