(ns game.mobs)

(def mobs-table
  [[:name        , :hp, :speed, :damage]
   ["an orc pawn", 110, 1     , 90]
   ["spiderling" , 100, 1     , 100]])

(defn table->maps [table]
  (for [row (next table)]
    (zipmap (first table) row)))

(def mobs (zipmap (range) (table->maps mobs-table)))
