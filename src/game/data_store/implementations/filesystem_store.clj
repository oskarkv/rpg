(ns game.data-store.implementations.filesystem-store
  (:require [game.data-store.protocols :as data-store]
            [game.core :as core]))

(deftype FilesystemStore [dir]
  ; Since it is concievable that some stores will need to be started and
  ; stopped, e.g. a database-backed store, all stores must implement the
  ; Lifecycle protocol, since code using the store should not have to know
  ; whether or not a particular store actually needs to be started and stopped.
  core/Lifecycle
  (start [this])
  (stop [this])
  data-store/DataStore
  (store [this key val]
    (spit (str dir "/" key) (pr-str val))
    val)
  (load [this key]
    (try
      (read-string (slurp (str dir "/" key)))
      (catch java.lang.RuntimeException e
        nil)
      (catch java.io.FileNotFoundException e
        nil))))
