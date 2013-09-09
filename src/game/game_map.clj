(ns game.game-map
  (:require [clojure.walk :as walk]))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn load-game-map []
  (let [size 3]
    (vectorize (partition size (repeat (* size size) 1)))))
