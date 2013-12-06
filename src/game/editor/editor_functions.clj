(ns game.editor.editor-functions
  (:require (game.common [graphics :as gfx])))

(defn create-spawn-location [editor-state]
  (let [coords (gfx/get-target-coords)]
    (if coords
      {:editor-state
       (assoc-in editor-state [:game-map :spawns 2] {:pos coords :type 0 :respawn-time 3})
       :event [:create-spawn-location]}
      {:editor-state editor-state})))
