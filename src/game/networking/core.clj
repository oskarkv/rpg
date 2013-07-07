(ns game.networking.core)

(defprotocol NetworkingSystem
  (start [this])
  (stop [this])
  (update [this]))

(defprotocol NetworkingConnection
  (send-reliably [this edn])
  (send-unreliably [this edn])
  (ping [this])
  (get-address [this]))
