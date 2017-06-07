(ns game.tile-sets
  (:require
   [clojure.core.matrix :as m]
   [clojure.set :as set]
   [game.game-map :as gmap]
   [game.math :as math]
   [game.utils :refer :all]))

(defn make-map
  "Make an x by y map (matrix), initialized with the optional value v. If v is
   not provided, use 0."
  ([x y] (make-map x y 0))
  ([x y v]
   (vec (repeat x (vec (repeat y v))))))

(defn find-bounds
  "Find the bounding rectangle of points. Returns [bottom top]. Bottom and top
   may include floating-point numbers."
  [points]
  (let [xs (map first points)
        ys (map second points)]
    [[(apply min xs) (apply min ys)]
     [(apply max xs) (apply max ys)]]))

(defn find-int-bounds [points]
  (m/emap int (find-bounds points)))

(defn poses-between
  "Returns a set of the tile positions that exist in a rectangle defined by the
   2d points bottom and top, inclusive. Top should have a larger x and a larger
   y than bottom."
  [bottom top]
  (let [[bx by] bottom
        [tx ty] (map inc top)]
    (set (for [y (range by ty)
               x (range bx tx)]
           [x y]))))

(defn poses-in-polygon
  "Find the poses inside the polygon defined by verts, a sequence of points.
   The verts must be in cyclic order."
  [verts]
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
         number (int (* ratio (* x y)))]
     (fill m (take number (shuffle (all-poses m))) v))))

(defn remove-illegal-poses
  "Returns only the positions of poses that are legal in m."
  [m poses]
  (let [[xs ys] (math/mat-size m)]
    (remove (fn [[x y]] (or (< x 0) (< y 0) (>= x xs) (>= y ys))) poses)))

(defn all-neighbors
  "Returns all neighbors of pos, including pos itself, at manhattan distance
   dist, in m. If dist is not provided, defaults to 1."
  ([m pos] (all-neighbors m 1 pos))
  ([m dist pos]
   (let [[x y] pos
         range* (range (- dist) (inc dist))]
     (remove-illegal-poses
      m (for [j range* i range*]
          [(+ x i) (+ y j)])))))

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
  "Returns a set of all poses connected with start-pos in m, via poses whose
   value in m satisfies test-fn. If test-fn is not provided, defaults to a set
   of the value of start-pos in m."
  ([m start-pos] (flood-fill m start-pos #{(get-in m start-pos)}))
  ([m start-pos test-fn]
   (loop [queue (conj empty-queue start-pos)
          visited #{start-pos}]
     (if-not (peek queue)
       visited
       (let [c (peek queue)
             adj (filter #(test-fn (get-in m %)) (cross-neighbors m c))
             new-adj (remove visited adj)]
         (recur (into (pop queue) new-adj) (into visited new-adj)))))))

(defn walls-and-rooms
  "Returns a map of :walls and :rooms, where walls is the set of all wall
   tiles, and rooms is a vector of connected sets of room tiles in m."
  [m]
  (let [all (all-poses m)
        {walls true other false} (group-by (gmap/wall-in?-fn m) all)]
    {:walls walls :rooms
     (loop [rooms [] left other]
       (if (seq left)
         (let [t (first left)
               room (flood-fill m t gmap/walkable-type?)]
           (recur (conj rooms room) (remove room left)))
         rooms))}))

(defn closest-pair
  "Returns a pair of points from two different collections, such that they are
   the approximately the closest pair between the two sets. Compares
   n/take-denom tiles of each room to each other."
  ([set1 set2] (closest-pair set1 set2 3))
  ([set1 set2 take-denom]
   (let [take-some #(take (/ (count %) 3) (shuffle %))
         [a b] (apply min-key
                      #(% 2)
                      (for [a (take-some set1) b (take-some set2)]
                        [a b (math/squared-distance a b)]))]
     [a b])))

(defn closest-pair
  "Returns a pair of points from different rooms, such that they are the
   approximately the closest pair between those two rooms, but not necessarily
   the closest pair globally between different rooms."
  [rooms]
  (let [[room & other] rooms
        others (apply concat other)
        take-some #(take (/ (count %) 3) (shuffle %))
        [a b] (apply min-key
                     #(% 2)
                     (for [a (take-some room) b (take-some others)]
                       [a b (math/squared-distance a b)]))]
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

(defn connect-rooms
  "Connect the rooms in m so that every walkable tile in m is reachable from
   every other."
  [m]
  (let [{:keys [walls rooms]} (walls-and-rooms m)]
    (loop [[room1 & others] rooms m m]
      (if (seq others)
        (let [room2 (first others)
              [a b] (closest-pair room1 room2)
              rect (rectangle-between-points a b 1.5 0.5)
              to-remove (filter #(math/inside? % rect) walls)
              new-m (fill m to-remove :floor)]
          (recur (cons (set/union room1 room2 (set to-remove)) (rest others))
                 new-m))
        m))))

(defn points-in-circle [center radius]
  (let [top (map #(+ % radius) center)
        bottom (map #(- % radius) center)]
    (remove #(> (math/distance % center) radius)
            (poses-between bottom top))))
