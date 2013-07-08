(ns game.data-store.core
  (:refer-clojure :exclude [load]))

(defprotocol DataStore
  (store [this key val])
  (load [this key]))
