(ns game.data-store.core)

(defprotocol DataStore
  (store [this key val])
  (load [this key]))
