(ns game.key-value-store.protocols
  (:refer-clojure :exclude [load]))

(defprotocol KeyValueStore
  (store [this key val])
  (load [this key]))
