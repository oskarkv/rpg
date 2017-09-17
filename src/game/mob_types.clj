(ns game.mob-types)

(def mobs-table
  ;; The first row are the defaults
  [{:hp 100 :speed 1 :damage 10}
   {:name "an orc pawn" :hp 110 :damage 9}
   {:name "spiderling"}
   {:name "a black wolf" :hp 90 :damage 12}])

(defn complete-maps [table]
  (for [row (next table)]
    (merge (first table) row)))

(def mobs (zipmap (range) (complete-maps mobs-table)))
