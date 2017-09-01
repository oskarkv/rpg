(ns game.game-map)

(def tile-types
  [:ground :wall :water :tree])

(def traversable? #{:ground})

(def intraversable? (complement traversable?))

(defn traversable-in?-fn [m]
  #(traversable? (get-in m %)))

(defn intraversable-in?-fn [m]
  (complement (traversable-in?-fn m)))

(defn adjust-pos [pos]
  (map #(+ 0.5 %) pos))

(defn load-game-map [game-map]
  (let [{:keys [terrain spawns start end]} game-map]
    {:terrain terrain
     :player-spawn (adjust-pos start)
     :spawns
     (zipmap
      (range)
      (map
       (fn [pos] {:pos (adjust-pos pos) :respawn-time 3
                  :mobs [{:mob {:type 0 :levels [1 2]
                                :drops [{:id 0 :chance 1}
                                        {:id 1 :chance 1 :quantity 3}]}
                          :rel-chance 2}
                         {:mob {:type 1 :levels [1 2]
                                :drops [{:id 2 :chance 1}
                                        {:id 1 :chance 1 :quantity 3}]}
                          :rel-chance 1}]})
       spawns))}))
