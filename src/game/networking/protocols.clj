(ns game.networking.protocols)

(defprotocol NetworkingConnection
  (send-reliably [this edn])
  (send-unreliably [this edn])
  (ping [this])
  (get-address [this])
  (get-connection-id [this]))
