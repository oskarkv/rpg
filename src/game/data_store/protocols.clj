(ns game.data-store.protocols
  (:refer-clojure :exclude [load]))

(defprotocol DataStore
  (store [this key val])
  (load [this key]))
