(ns game.data-store.core
  (:require [game.data-store.implementations.filesystem-store :as impl]))

(defn construct-data-store []
  (impl/->FilesystemStore "data"))
