(ns game.mobs)

(def mobs-table
  [{:hp 100 :speed 1 :damage 10}
   {:name "an orc pawn" :hp 110 :damage 9}
   {:name "spiderling"}])

(defn complete-maps [table]
  (for [row (next table)]
    (merge (first table) row)))

(def mobs (zipmap (range) (complete-maps mobs-table)))
