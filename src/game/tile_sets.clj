(ns game.tile-sets
  (:require
   [clojure.core.matrix :as m]
   [clojure.set :as set]
   [game.game-map :as gmap]
   [game.math :as math])
  (:use game.utils))

(defn make-map
  "Make an x by y map (matrix), initialized with the optional value v. If v is
   not provided, use the value for floor."
  ([x y] (make-map x y :floor))
  ([x y v]
   (vec (repeat x (vec (repeat y (gmap/tile-type v)))))))

(defn find-bounds
  "Find the bounding rectangle of points."
  [points]
  (let [xs (map first points)
        ys (map second points)]
    [[(apply min xs) (apply min ys)]
     [(apply max xs) (apply max ys)]]))

(defn find-int-bounds [points]
  (let [[bottom top] (find-bounds points)]
    [(mapv int bottom)
     (mapv (comp int math/ceil) top)]))

(defn poses-between
  "Returns a set of the tile positions that exist in a rectangle defined by the
   2d points bottom and top, inclusive. Note that top should have a larger x
   and a larger y than bottom."
  [bottom top]
  (let [[bx by] bottom
        [tx ty] (map inc top)]
    (set (for [y (range by ty)
               x (range bx tx)]
           [x y]))))

(defn poses-in-polygon [verts]
  (let [[bottom top] (find-int-bounds verts)
        poses (poses-between bottom top)]
    (filter #(math/inside? % verts) poses)))

(defn all-poses [m & {:keys [indent bottom top]
                      :or {indent 0 bottom [0 0]
                           top (map dec (math/mat-size m))}}]
  (let [bottom (map #(+ % indent) bottom)
        top (map #(- % indent) top)]
    (poses-between bottom top)))

(defn fill
  ([m poses] (fill m poses :wall))
  ([m poses v]
   (reduce (fn [m path] (assoc-in m path (gmap/tile-type v)))
           m poses)))

(defn fill-randomly
  "Fill ratio of m randomly with the value v (defaults to the wall value if
   not provided)."
  ([m ratio] (fill-randomly m ratio :wall))
  ([m ratio v]
   (let [[x y] (math/mat-size m)
         number (math/floor (* ratio (* x y)))]
     (fill m (take number (shuffle (all-poses m))) v))))

(defn remove-illegal-poses
  "Returns only the positions of poses that are legal in m."
  [m poses]
  (let [[xs ys] (math/mat-size m)]
    (remove (fn [[x y]] (or (< x 0) (< y 0) (>= x xs) (>= y ys))) poses)))

(defn all-neighbors
  "Returns all neighbors of pos, including pos itself, at manhattan distance
   dist, in m."
  [m dist pos]
  (let [[x y] pos
        range* (range (- dist) (inc dist))]
    (remove-illegal-poses
     m (for [j range* i range*]
         [(+ x i) (+ y j)]))))

(defn cross-neighbors
  "Returns the neighbors, in a cross, of pos in m. If more positions, returns
   the union of their neighbors."
  ([m pos]
   (let [[x y] pos]
     (set (remove-illegal-poses
           m [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))))
  ([m pos & more]
   (set/union (cross-neighbors m pos) (apply cross-neighbors m more))))

(defn flood-fill
  "Returns a set of all room tiles connected to start-pos in m."
  [m start-pos]
  (loop [q (conj empty-queue start-pos)
         v #{start-pos}]
    (if-not (peek q)
      v
      (let [c (peek q)
            adj (remove (gmap/wall-in? m) (cross-neighbors m c))
            new-adj (remove v adj)]
        (recur (into (pop q) new-adj) (into v new-adj))))))

(defn walls-and-rooms
  "Returns a map of :walls and :rooms, where walls is the set of all wall
   tiles, and rooms is a vector of connected sets of room tiles in m."
  [m]
  (let [all (all-poses m)
        {walls true other false} (group-by (gmap/wall-in? m) all)]
    {:walls walls :rooms
     (loop [rooms [] left other]
       (if (seq left)
         (let [t (first left)
               room (flood-fill m t)]
           (recur (conj rooms room) (remove room left)))
         rooms))}))

(defn closest-pair
  "Returns a pair of points from different rooms, such that they are the
   approximately the closest pair between those two rooms, but not necessarily
   the closest pair globally between different rooms."
  [rooms]
  (let [[room & other] rooms
        others (apply concat other)
        take-some #(take (/ (count %) 3) (shuffle %))
        [a b] (first (sort-by
                      #(% 2)
                      (for [a (take-some room) b (take-some others)]
                        [a b (math/distance a b)])))]
    [a b]))

(defn rectangle-between-points
  "Returns a rectangle (defined by 4 points) around p1 and p2, as if by first
   extending the line segment between p1 and p2 by extra on each end, and then
   widening the line."
  [p1 p2 width extra]
  (let [fw (math/norm-diff p2 p1)
        bw (m/sub fw)
        [e-> e<-] (map #(m/mul % extra) [fw bw])
        [v1 v2] (map #(m/mul % (/ width 2.0))
                     (let [[x y] fw] [[y (- x)] [(- y) x]]))]
    [(m/add p1 v1 e<-) (m/add p1 v2 e<-) (m/add p2 v2 e->) (m/add p2 v1 e->)]))

(defn connect-rooms [m]
  (let [{:keys [walls rooms]} (walls-and-rooms m)]
    (if (> (count rooms) 1)
      (let [[a b] (closest-pair rooms)
            rect (rectangle-between-points a b 1.5 0.5)
            to-remove (filter #(math/inside? % rect) walls)
            new-m (fill m to-remove :floor)]
        (recur new-m))
      m)))

(defn points-in-circle [center radius]
  (let [top (map #(+ % radius) center)
        bottom (map #(- % radius) center)]
    (remove #(> (math/distance % center) radius)
            (poses-between bottom top))))
