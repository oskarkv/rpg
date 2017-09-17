(ns game.server.inventory
  (:require
   [game.inventory :as inv]
   [game.server.base :as b]))

(defmethod b/process-event :changed-gear [game-state {:keys [id]}]
  (b/update-player game-state id))

(defmethod b/process-event :c-rearrange-inv [game-state {:keys [id paths]}]
  (let [real-paths (map #(into [:chars id] %) paths)]
    (inv/inv-swap game-state real-paths b/enqueue-events id)))

(defmethod b/process-event :c-move-quantity
  [game-state {:keys [id from-path to-path quantity] :as event}]
  (let [[from to] (map #(into [:chars id] %) [from-path to-path])]
    (inv/move-quantity game-state from to quantity)))

(defmethod b/process-event :c-destroy-item
  [game-state {:keys [id path quantity]}]
  (let [sv-path (into [:chars id] path)]
    (inv/destroy-item game-state sv-path quantity)))
