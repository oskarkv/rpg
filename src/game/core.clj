(ns game.core)

(defprotocol Lifecycle
  (start [this])
  (stop [this]))

(defprotocol Updatable
  (update [this]))
