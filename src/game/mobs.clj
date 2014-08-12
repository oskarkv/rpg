(ns game.mobs)

(def mobs-table
  [{:hp 100 :speed 1 :dmg 10}
   {:name "an orc pawn" :hp 110 :dmg 9}
   {:name "spiderling"}
   {:name "a black wolf" :hp 90 :dmg 12}])

(defn complete-maps [table]
  (for [row (next table)]
    (merge (first table) row)))

(def mobs (zipmap (range) (complete-maps mobs-table)))
