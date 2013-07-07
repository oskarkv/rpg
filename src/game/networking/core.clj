(ns game.networking.core)

(defprotocol NetworkingConnection
  (send-reliably [this edn])
  (send-unreliably [this edn])
  (ping [this])
  (get-address [this]))
