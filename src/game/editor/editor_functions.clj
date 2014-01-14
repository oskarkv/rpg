(ns game.editor.editor-functions
  (:require (game.key-value-store [protocols :as kvs])
            (game.common [graphics :as gfx])
            (game [constants :as consts])))

(defn init-spawn-id-counter [start-id]
  (let [spawn-id-counter (atom start-id)]
    (defn next-spawn-id []
     (swap! spawn-id-counter inc))))

(defn create-spawn-location [game-state]
  (let [coords (gfx/get-target-coords)]
    (if coords
      {:new-game-state
       (assoc-in game-state
                 [:game-map :spawns (next-spawn-id)]
                 {:pos coords :type 0 :respawn-time 3})
       :event [:create-spawn-location]})))

(defn save-zone [game-state key-value-store]
  (kvs/store
    key-value-store (str consts/zone-folders "gfay") (:game-map game-state)))

