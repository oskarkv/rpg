(ns game.game-map
  (:require [clojure.walk :as walk]))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn load-game-map []
  (vectorize (partition 20 (repeat (* 20 20) 1))))
