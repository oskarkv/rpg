(ns game.server.mobs-and-looting
  (:require
   [game.common.core-functions :as ccfns]
   [game.inventory :as inv]
   [game.items :as items]
   [game.constants :as consts]
   [game.mob-types :as mt]
   [game.server.base :as b]
   [game.utils :refer :all]))

(defn roll-for-mob [mobs]
  (random-pick (into {} (map #(vector % (:rel-chance %))) mobs)))

(defn roll-for-drops [drops]
  (for [item drops
        :let [q (or (:quantity item) 1)
              actual (rand-binomial q (:chance item))]
        :when (< 0 actual)]
    {:id (:id item) :quantity actual}))

(defn spawn-mob [spawn-id spawns]
  (let [mob (-> spawn-id spawns :mobs roll-for-mob :mob)
        mob-type (-> mob :type mt/mobs)
        [min max] (:levels mob)
        level (+ min (rand-int (- (inc max) min)))
        drops (roll-for-drops (:drops mob))
        ;; This is wrong, but in the middle of reworking drops
        drops-with-stats drops
        unstacked (vec (mapcat items/unstack drops-with-stats))]
    (assoc mob-type
           :type :mob
           :spawn spawn-id
           :pos (-> spawn-id spawns :pos)
           :move-dir [0 0]
           :last-attack 0
           :max-hp (:hp mob-type)
           :delay 1
           :level 1
           :drops unstacked)))

(defmethod b/process-event :spawn-ids
  [{:keys [to-spawn spawns player-ids] :as game-state} {ids :ids}]
  (let [new-mobs (map #(spawn-mob % spawns) ids)
        new-mobs-map (zipmap (repeatedly b/new-game-id) new-mobs)
        num-mobs (count new-mobs-map)]
    (b/enqueue-msgs [player-ids
                     {:type :s-spawn-mobs
                      :mobs (b/prepare-chars-for-sending new-mobs-map)}])
    (-> game-state
      (update :chars merge new-mobs-map)
      (assoc :to-spawn (call-times num-mobs pop to-spawn)))))

(defmethod b/process-event :c-loot-corpse [game-state {:keys [id corpse-id]}]
  (when (ccfns/id-close-enough? game-state id corpse-id consts/loot-distance)
    (b/enqueue-msgs [[id] {:type :s-loot :corpse-id corpse-id
                           :drops (get-in game-state
                                          [:corpses corpse-id :drops])}])
    (update-in game-state [:corpses corpse-id :looting]
               #(or (some-> % (conj id)) #{id}))))

(defmethod b/process-event :c-quit-looting [game-state {:keys [id ids]}]
  (reduce (fn [gs cid]
            (update-in gs [:corpses cid :looting] disj id))
          game-state
          ids))

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

(defmethod b/process-event :decay-corpses [game-state {:keys [ids]}]
  (b/enqueue-msgs [(:player-ids game-state) {:type :s-decay-corpses :ids ids}])
  (update game-state :corpses #(apply dissoc % ids)))

(defn check-spawns [{:keys [to-spawn] :as game-state}]
  (let [curr-time (current-time-ms)
        time-to-spawn (fn [[id spawn-time]] (> curr-time spawn-time))
        ids (keys (take-while time-to-spawn to-spawn))]
    (when (seq ids)
      (b/enqueue-events {:type :spawn-ids :ids ids}))))

(defn check-corpses [{:keys [corpses] :as game-state}]
  (let [curr-time (current-time-ms)
        time-to-decay (fn [[id {:keys [decay-time]}]] (> curr-time decay-time))
        ids (keys (take-while time-to-decay corpses))]
    (when (seq ids)
      (b/enqueue-events {:type :decay-corpses :ids ids}))))
