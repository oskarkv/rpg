(ns game.common.items
  (:import (java.util Random))
  (:require [clojure.math.numeric-tower :as math]))

(defn adjusting-fn [mean target]
  (if (> mean target)
    #(* (/ target mean) %)
    #(+ % (* (- 1 %) (/ (- target mean) (- 1 mean))))))

(defn random-variables-with-mean [mean num-vars]
  (let [vars (repeatedly num-vars rand)
        vars-mean (/ (apply + vars) num-vars)]
    (map (adjusting-fn vars-mean mean) vars)))

(let [r (Random.)]
  (defn rand-gaussian
    ([] (rand-gaussian 1 0))
    ([sd] (rand-gaussian sd 0))
    ([sd mean] (+ (* sd (.nextGaussian r)) mean))))
