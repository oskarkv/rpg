(ns game.game-map
  (:require [clojure.walk :as walk]
            (game [mobs :as mobs]
                  [dungeon-generator :as dg])))

(defn walkable-type? [type]
  (not= type (:wall dg/tile-types)))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn adjust-pos [pos]
  (map #(+ 0.5 %) pos))

(defn load-game-map []
  (let [{:keys [terrain spawns start end]} (dg/make-round-rooms 2 10 10 0.45)]
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
