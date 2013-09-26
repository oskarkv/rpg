(ns game.game-map
  (:require [clojure.walk :as walk]))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn load-game-map []
  {:terrain
   (let [size 3]
     (vectorize (partition size (repeat (* size size) 1))))
   :spawns (zipmap (range) [{:pos [2 1] :respawn-time 3 :name "an orc pawn"}
                            {:pos [1 3] :respawn-time 3 :name "an orc pawn"}])})
