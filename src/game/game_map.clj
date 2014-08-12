(ns game.game-map
  (:require [clojure.walk :as walk]
            (game [mobs :as mobs])))

(defn walkable-type? [type]
  (= type 1))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn load-game-map []
  {:terrain
   (let [size 5]
     (assoc-in (vectorize (partition size (repeat (* size size) 1)))
               [2 2] 0))
   :spawns (zipmap (range)
                   [{:pos [1 1] :respawn-time 3
                     :mobs [{:mob {:type 0 :levels [1 2]
                                   :drops [{:id 0 :chance 1}
                                           {:id 1 :chance 1 :quantity 3}]}
                             :rel-chance 2}
                            {:mob {:type 1 :levels [1 2]
                                   :drops [{:id 2 :chance 1}
                                           {:id 1 :chance 1 :quantity 3}]}
                             :rel-chance 1}]}])})
