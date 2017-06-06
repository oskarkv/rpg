(ns game.key-value-store.core
  (:require
   [game.key-value-store.implementations.filesystem-kvs :as impl]))

(defn construct-key-value-store []
  (impl/->FilesystemKeyValueStore "data"))
