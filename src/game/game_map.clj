(ns game.game-map)

(def tile-types
  [:ground :wall :water :tree :stone])

(def traversable? #{:ground})

(def intraversable? (complement traversable?))

(defn traversable-in?-fn [m]
  #(traversable? (get-in m %)))

(defn intraversable-in?-fn [m]
  (complement (traversable-in?-fn m)))

(defn adjust-pos [pos]
  (map #(+ 0.5 %) pos))

(defn load-game-map [game-map]
  (let [{:keys [terrain spawns player-spawn]} game-map]
    {:terrain terrain
     :player-spawn (adjust-pos player-spawn)
     :spawns
     (zipmap
      (range)
      (map (fn [pos] {:pos (adjust-pos pos) :level 1})
           spawns))}))
