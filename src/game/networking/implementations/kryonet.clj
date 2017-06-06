(ns game.networking.implementations.kryonet
  (:require
   [game.common.core :as cc]
   [game.networking.protocols :as net]
   [game.utils :refer :all])
  (:import
   (com.esotericsoftware.kryonet Client Connection Listener Server)
   (java.net InetAddress)))

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

(defmacro defnetworkingsystem [name args & start-body]
  `(deftype- ~name ~args
     cc/Lifecycle
     (~'start [this#]
      ~@start-body)
     (~'stop [this#]
      (.stop ~(first args)))
     cc/Updatable
     (~'update [this# _#]
      ;; Maybe having a positive timeout is better.
      (.update ~(first args) 0))))

(defnetworkingsystem KryonetServer [server port]
  ;; .bind must be called in a thread other than the update thread, but
  ;; update needs to be called at the same time; blocks until bound.
  (.start server)
  (.bind server port))

(defnetworkingsystem KryonetClient [client address port]
  ;; .connect must be called in a thread other than the update thread, but
  ;; update needs to be called at the same time; blocks until connected.
  (.start client)
  (.connect client 5000 (InetAddress/getByName address) port))

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

(let [buffer-size 50000]
  (defn construct-server [port conn-fn recv-fn disc-fn]
    (let [server (Server. buffer-size buffer-size)]
      {:net-sys (construct-system server ->KryonetServer
                                  [port] conn-fn recv-fn disc-fn)}))
  (defn construct-client [address port conn-fn recv-fn disc-fn]
    (let [client (Client. buffer-size buffer-size)]
      {:net-sys (construct-system client ->KryonetClient
                                  [address port] conn-fn recv-fn disc-fn)
       :conn client})))
