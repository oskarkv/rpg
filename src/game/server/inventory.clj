(ns game.server.inventory
  (:require
   [game.common.core-functions :as ccfns]
   [game.constants :as consts]
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

(defn can-loot? [game-state id path]
  (when-let [corpse-id (path 1)]
    (and (some-> (get-in game-state [:corpses corpse-id :tagged-by]) (= id))
         (ccfns/id-close-enough? game-state id corpse-id
                                 consts/loot-distance))))

(defn update-looting [game-state corpse-id]
  (->> (get-in game-state [:corpses corpse-id :looting])
    (filter #(ccfns/id-close-enough? game-state % corpse-id
                                     consts/loot-distance))
    set))

(defn loot-item [game-state id from-path]
  (let [ngs (inv/loot-item game-state from-path [:chars id :inv])
        looting (get-in ngs [:corpses (from-path 1) :looting])]
    (b/enqueue-msgs [[id] {:type :s-loot-item-ok :from-path from-path}]
                    [(disj looting id)
                     {:type :s-item-looted :path from-path
                      :left (:quantity (get-in ngs from-path))}])
    ngs))

(defmethod b/process-event :c-loot-item [game-state {:keys [id from-path]}]
  (when (can-loot? game-state id from-path)
    (let [corpse-id (from-path 1)
          ngs (assoc-in game-state [:corpses corpse-id :looting]
                        (update-looting game-state corpse-id))]
      (loot-item ngs id from-path))))
