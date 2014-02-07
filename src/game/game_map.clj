(ns game.game-map
  (:require [clojure.walk :as walk]))

(defn walkable-type? [type]
  (= type 1))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn load-game-map []
  {:terrain
   (let [size 5]
     (assoc-in (vectorize (partition size (repeat (* size size) 1)))
               [2 2] 0))
   :spawns (zipmap (range) [{:pos [3 4] :respawn-time 3 :type 0}
                            {:pos [4 3] :respawn-time 3 :type 1}])})
