(ns game.server.mobs-and-looting
  (:require
   [game.common.core-functions :as ccfns]
   [game.constants :as consts]
   [game.item-generation :as ig]
   [game.items :as items]
   [game.mob-types :as mt]
   [game.server.base :as b]
   [game.stats :as stats]
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
  (let [{:keys [pos level]} (spawns spawn-id)
        mob-type (rand-nth (vals mt/mobs))]
    (assoc mob-type
           :type :mob
           :spawn spawn-id
           :pos pos
           :level level
           :move-dir [0 0]
           :last-attack 0
           :max-hp (:hp mob-type)
           :delay 1
           :drops (some-> (ig/generate-item level) vector))))

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
