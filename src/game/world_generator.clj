(ns game.world-generator
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [game.math :as math]
   [game.matrix-visualizer :as mvis]
   [game.tile-sets :as ts]
   [game.utils :refer :all]
   [game.voronoi :as vor]
   [loco.constraints :refer :all]
   [loco.core :as loco]
   [loom.alg :as lalg]
   [loom.attr :as lattr]
   [loom.graph :as loom]
   [loom.io :as lio]
   [loom.label :as llabel]))

(defn distance [g k k2]
  (dec (count (lalg/bf-path g k k2))))

(defn make-model [graph loom-graph]
  (let [gks (keys graph)
        n (count graph)
        hn (int (math/ceil (/ n 2)))
        card-map (zipmap (range 1 (inc (int (/ n 2)))) (repeat 2))]
    (concat
     ;; How many of each can there be
     [($cardinality gks card-map)]
     ;; The range for each variable
     (for [k gks]
       ($in k 1 hn))
     ;; If k is not max level, a neighbor must be k + 1
     (for [k gks]
       ($if ($< k hn)
            (apply $or
                   (for [k2 (graph k)]
                     ($= k2 ($+ k 1))))))
     ;; If k is 1 or 2, the other 1 or 2 must not be close
     (for [k gks
           k2 gks
           :when (not= k k2)]
       ($if ($or ($= k k2 1) ($= k k2 2))
            ($> (distance loom-graph k k2) 2)))
     ;; A neighbor can not be the same level
     (for [k gks]
       ($not (apply $or
                    (for [k2 (graph k)]
                      ($= k k2))))))))

(defn add-labels [g node label & more]
  (let [ng (lattr/add-attr g node :label label)]
    (if (seq more)
      (apply add-labels ng more)
      ng)))

(defn draw-graph [graph]
  (let [graph (walk/postwalk (fn [n] (if (number? n) [:x n] n)) graph)
        loom-graph (loom/graph graph)
        sols (time [(loco/solution (make-model graph loom-graph))])]
    (println (count sols))
    (dotimes [x (count sols)]
      (let [sol (nth sols x)
            g (apply add-labels (loom/graph graph)
                     (mapcat #(list %1 [%1 %2])
                             (keys graph)
                             (map sol (keys graph))))]
        (lio/view g)))))
