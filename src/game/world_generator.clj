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

;;; These 3 fns have to do with loco (constraint programming) and loom

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

;;; These fns are about deciding levels without loco

(defn invert-and-group
  "Inverts a map. The vals in the returned map are vectors of keys in the input
   map."
  [m]
  (group-by m (keys m)))

(defn distance-map
  "Returns a map from pairs to distances between the nodes."
  [graph pairs]
  (into {} (map #(vector % (apply distance graph %))) pairs))

(defn path-score [levels graph]
  (let [inv-levels (invert-and-group levels)
        [a b] (inv-levels 1)
        path
        (fn [successors]
          (fn path [start]
            (let [lvl (levels start)
                  conts (filter #(some-> (levels %) (== (inc lvl)))
                                (set (successors graph start)))
                  c (count conts)]
              (cons start
                    (condp == c
                      0 nil
                      1 (path (first conts))
                      (apply max-key count (map path conts)))))))
        with-jump (path (fn [g s]
                          (->> (fn [s]
                                 (let [lvl (levels s)]
                                   (filter #(<= (levels %) lvl) (g s))))
                            (bfs s)
                            (mapcat g))))
        no-jump (path (fn [g s] (g s)))]
    (transduce (map count) +
               [(with-jump a) (no-jump a) (with-jump b) (no-jump b)])))

(defn fairness-score
  "Compares the distance from the level 1 zones to the level 2 zones, to the
   level 3 zones and so on, and gives a higher score to configurations that are
   more fair."
  [levels graph]
  (let [inv-levels (invert-and-group levels)
        [a b] (inv-levels 1)
        pairs (all-set-pairs (keys levels))
        dist-map (distance-map (loom/graph graph) pairs)
        max-dist (apply max (vals dist-map))]
    (reduce (fn [score level]
              (let [places (inv-levels level)
                    dists (map (fn [s]
                                 (sort (map #(dist-map #{% s}) places)))
                               [a b])]
                (apply + score
                       (apply map (comp #(- max-dist %) math/abs -) dists))))
            0
            (set (remove #{1} (keys inv-levels))))))

(defn separation-score
  "Returns the distance between the level 1 zones."
  [levels graph]
  (let [inv-levels (invert-and-group levels)
        [a b] (inv-levels 1)]
    (distance (loom/graph graph) a b)))

(defn levels-along-path
  "Assign levels in levels-map from 1 to n to the n first nodes in the path
   from a to b. g must be a loom graph."
  [levels-map g [a b] n]
  (let [path (take n (lalg/bf-path g a b))]
    (reduce (fn [levels [idx z]]
              (assoc levels z (inc idx)))
            levels-map
            (indexed path))))

;; This fn has two problems. First, its backtracking only ever goes down in
;; levels, even thought it should be allowed to go up, when the resulting zone
;; has a lower level than the start zone. Second, the backtracking can go to the
;; second start positions zones, not really backtracking at all.
;;   I should probably try to make a simpler version, maybe with the help of
;; bfs.
(defn find-next-zone [levels graph start current]
  (when-not (nil? current)
    (let [clvl (levels current)
          possibilities (remove #(some? (levels %)) (graph current))]
      (if-not (zero? (count possibilities))
        (let [g (loom/graph graph)]
          (first (sort-by #(distance g start %) possibilities)))
        (->> (graph current)
          (remove (some-fn #(nil? (levels %)) #(>= (levels %) clvl)))
          (sort-by levels >)
          first
          (find-next-zone levels graph start))))))

(defn fill-levels
  "Fills levels with levels, one level at a time (a pair of zones at a time),
   using find-next-zone. Returns the filled levels map."
  [levels graph [a-start b-start]]
  (let [inv-levels (invert-and-group levels)
        max-level (apply max (keys inv-levels))
        [a b] (inv-levels max-level)]
    (loop [levels levels curr-a a curr-b b next-level (inc max-level)]
      (if-lets [na (find-next-zone levels graph a-start curr-a)
                levels (assoc levels na next-level)
                nb (find-next-zone levels graph b-start curr-b)
                levels (assoc levels nb next-level)]
        (recur levels na nb (inc next-level))
        levels))))

;; This fn can fail to assign a level to all zones, because find-next-zone's
;; backtracking only ever goes down in levels, even when going up would result
;; in reaching a zone with a level that is lower than the original.
(defn decide-levels [graph edge-zones tries]
  (letfn [(decide [_]
            (let [g (loom/graph graph)
                  pairs (all-set-pairs edge-zones)
                  dist-map (distance-map g pairs)
                  inv-dist (invert-and-group dist-map)
                  max-dist (apply max (keys inv-dist))
                  start-pair (vec (rand-nth (inv-dist (min max-dist 5))))
                  levels (levels-along-path {} g start-pair 2)
                  levels (levels-along-path levels g (reverse start-pair) 2)]
              (fill-levels levels graph start-pair)))]
    (apply max-key #(fairness-score % graph) (map decide (range tries)))))
