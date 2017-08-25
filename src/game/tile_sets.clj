(ns game.tile-sets
  (:require
   [clojure.core.matrix :as m]
   [clojure.set :as set]
   [game.game-map :as gmap]
   [game.math :as math]
   [game.utils :refer :all]))

(defn make-map
  "Make an x by y map (matrix), initialized with the optional value v. If v is
   not provided, use :wall."
  ([[x y]] (make-map x y :wall))
  ([x y] (make-map x y :wall))
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

(defn area-between
  "Returns a set of the tiles that exist in a rectangle defined by the 2d points
   bottom (inclusive) and top (exclusive). Top should have a larger x and a
   larger y than bottom."
  [bottom top]
  (let [[bx by] bottom
        [tx ty] top]
    (set (for [y (range by ty)
               x (range bx tx)]
           [x y]))))

(defn tiles-in-polygon
  "Find the tiles inside the polygon defined by verts, a sequence of points.
   The verts must be in cyclic order."
  [verts]
  (let [[bottom top] (find-int-bounds verts)
        tiles (area-between bottom top)]
    (filter #(math/inside? % verts) tiles)))

(defn all-tiles [m & {:keys [indent bottom top]
                      :or {indent 0 bottom [0 0]
                           top (math/mat-size m)}}]
  (let [bottom (map #(+ % indent) bottom)
        top (map #(- % indent) top)]
    (area-between bottom top)))

(defn fill
  ([m tiles] (fill m tiles :wall))
  ([m tiles v]
   (reduce (fn [m path] (assoc-in m path v))
           m tiles)))

(defn fill-randomly
  "Fill ratio of m randomly with the value v (defaults to the wall value if
   not provided)."
  [m ratio & {:keys [value area] :or {value :wall area (all-tiles m)}}]
  (let [number (int (* ratio (count area)))]
    (fill m (take number (shuffle area)) value)))

(defn remove-illegal-tiles
  "Returns a seq of only the tiles that are legal in m. Returns a transducer if
   called with 1 argument."
  ([m]
   (let [[xs ys] (math/mat-size m)]
     (remove (fn [[x y]] (or (< x 0) (< y 0) (>= x xs) (>= y ys))))))
  ([m tiles]
   (into #{} (remove-illegal-tiles m) tiles)))

(defn all-neighbors
  "Returns all neighbors of tile, including tile itself, in a 2 * dist + 1
   square around tile. If dist is not provided, defaults to 1."
  ([tile] (all-neighbors tile 1))
  ([tile dist]
   (let [[x y] tile
         range* (range (- dist) (inc dist))]
     (for [j range* i range*]
       [(+ x i) (+ y j)]))))

(defn legal-neighbors
  ([m tile] (legal-neighbors m tile 1))
  ([m tile dist] (remove-illegal-tiles m (all-neighbors tile dist))))

(defn cross-neighbors
  "Returns the 4 neighbors, in a cross, of tile. If more tiles, returns the
   union of their neighbors."
  ([tile]
   (let [[x y] tile]
     (set [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))
  ([tile & more]
   (apply set/union (cross-neighbors tile) (map cross-neighbors more))))

(defn legal-cross-neighbors [m & tiles]
  (remove-illegal-tiles m (apply cross-neighbors tiles)))

(defn flood-fill
  "Flood fills starting from start. The successors of a node is given by
   (successors node). Ends when no new nodes are discovered. Returns the set
   of visited nodes."
  [start successors]
  (loop [queue (conj empty-queue start)
         visited #{start}]
    (if-not (peek queue)
      visited
      (let [current (peek queue)
            adj (successors current)
            new-adj (remove visited adj)]
        (recur (into (pop queue) new-adj) (into visited new-adj))))))

(defn same-val-as-start [m start-tile]
  (let [start-tile-value (get-in m start-tile)]
    (fn [tile]
      (= start-tile-value (get-in m tile)))))

(defn flood-fill-map
  "Returns a set of all tiles connected with start-tile in m, via tiles whose
   value in m satisfies test-fn. If test-fn is not provided, defaults to a fn
   that returns true for tiles that have the same value in m as start-tile."
  ([m start-tile]
   (flood-fill-map m start-tile (same-val-as-start m start-tile)))
   ([m start-tile test-fn]
    (flood-fill start-tile #(filter test-fn (legal-cross-neighbors m %)))))

(defn flood-fill-zone
  ([m zone start-tile]
   (flood-fill-zone m zone start-tile (same-val-as-start m start-tile)))
  ([m zone start-tile test-fn]
   (flood-fill-map m start-tile (every-pred? test-fn (set zone)))))

(defn walls-and-rooms
  "Returns a map of :walls and :rooms, where walls is the set of all wall tiles
   in m (or zone), and rooms is a vector of connected sets of room tiles in m
   (or zone)."
  ([m] (walls-and-rooms m (all-tiles m)))
  ([m zone]
   (let [{walls true other false} (group-by (gmap/intraversable-in?-fn m) zone)]
     {:walls walls :rooms
      (loop [rooms [] left other]
        (if (seq left)
          (let [t (first left)
                room (flood-fill-map m t gmap/traversable?)]
            (recur (conj rooms room) (remove room left)))
          rooms))})))

(defn closest-pair
  "Returns a pair of points from two different collections, such that they are
   the approximately the closest pair between the two sets. Compares
   n/take-denom tiles of each room to each other."
  ([set1 set2] (closest-pair set1 set2 3))
  ([set1 set2 take-denom]
   (let [take-some #(take (/ (count %) take-denom) (shuffle %))
         [a b] (apply min-key
                      #(% 2)
                      (for [a (take-some set1) b (take-some set2)]
                        [a b (math/squared-distance a b)]))]
     [a b])))

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

(defn points-in-circle [center radius]
  (let [top (map (comp int math/ceil #(+ % radius)) center)
        bottom (map (comp int #(- % radius)) center)]
    (remove #(> (math/distance % center) radius)
            (area-between bottom top))))

(defn unsafe-outer-border [tiles]
  (math/difference (apply cross-neighbors tiles) tiles))

(defn outer-border [m tiles]
  (remove-illegal-tiles m (unsafe-outer-border tiles)))

(defn inner-border [tiles]
  (math/intersection (apply cross-neighbors (unsafe-outer-border tiles)) tiles))

(defn shrink-zone
  ([zone] (math/difference zone (inner-border zone)))
  ([zone times] (call-times times shrink-zone zone)))

(defn grow-zone
  ([zone] (math/union zone (unsafe-outer-border zone)))
  ([zone times] (call-times times grow-zone zone)))

(defn connected-sets
  "Given a collection of points, return a seq of connected sets, sorted with
   largest set first."
  [points]
  (if (seq points)
    (let [first-set (flood-fill
                     (first points)
                     #(math/intersection points (cross-neighbors %)))]
      (->> (cons first-set (connected-sets (remove first-set points)))
        (sort-by count >)))
    nil))

(defn heal-zone-map
  "Given a list of zones (possibly disconnected sets of tiles), merge small
   connected components into larger ones, and return a seq of connected zones,
   as many as the input zones."
  [zones]
  (let [n (count zones)
        all-areas (sort-by count < (mapcat connected-sets zones))]
    (loop [all all-areas]
      (if (= n (count all))
        all
        (let [s (first all)
              sn (apply cross-neighbors s)
              [before after] (split-with #(empty? (set/intersection sn %))
                                         (rest all))]
          (recur (concat before (cons (set/union s (first after))
                                      (rest after)))))))))

(defn pairs-to-graph [pairs]
  (reduce (fn [graph [a b]]
            (-> graph
              (update a conj b)
              (update b conj a)))
          {}
          pairs))

(def keep-biggest-connected (comp first connected-sets))

(defn shrink-and-discard
  "Shrink the zone by removing the outer tiles, amount times. Keep only the
   biggest connected component."
  ([zone] (shrink-and-discard zone 1))
  ([zone amount]
   (-> (shrink-zone zone amount)
     keep-biggest-connected)))

(defn find-border
  "Find the tiles that are part of the border between area1 and area2. The
   border is the tiles in area1 that are next to area2, and vice versa."
  [area1 area2]
  (let [[b1 b2] (map unsafe-outer-border [area1 area2])]
    (set/union (math/intersection area1 b2)
               (math/intersection area2 b1))))

(defn square-around-origin
  "Returns a set of tiles that make up a square of size size around the
   origin. If size is even, the square will lean toward the first quadrant."
  [size]
  (let [top (inc (int (/ size 2)))
        bot (int (/ (dec size) -2))]
    (area-between [bot bot] [top top])))

(defn widen-tile [tile size]
  (map #(mapv + tile %) (square-around-origin size)))

(defn widen-path [path width]
  (set (mapcat #(widen-tile % width) path)))

(defn path-between
  "Returns sequence of tiles that constitute a minimal path from t1 to t2.
   If width is provided, make the path that wide."
  ([t1 t2]
   (let [[x y] (map - t2 t1)
         deltas (uneven-interleave
                 (repeat (math/abs x) [(math/sign x) 0])
                 (repeat (math/abs y) [0 (math/sign y)]))]
     (reductions (fn [tile delta]
                   (mapv + tile delta))
                 t1
                 deltas)))
  ([t1 t2 width]
   (let [path (path-between t1 t2)]
     (if (== 1 width)
       path
       (widen-path path width)))))

(defn find-connections
  "Given a collection of zones (sets of tiles) and an integer tile-limit,
   calculates the connections between the zones. Zone a are connected to zone b
   if tile-limit tiles of a are adjacent to tiles in b. Returns a set of pairs."
  ([zones] (find-connections zones 1))
  ([zones tile-limit]
   (let [n (count zones)
         zone-map (zipmap (range) zones)
         border-map (zipmap (range) (map unsafe-outer-border zones))
         enough? (fn [z1 z2] (>= (count (math/intersection (border-map z1)
                                                           (zone-map z2)))
                                 tile-limit))]
     (set (for [z1 (range n) z2 (range (inc z1) n)
                :when (and (enough? z1 z2) (enough? z2 z1))]
            [z1 z2])))))

(defn connect-zones
  ([m zone1 zone2] (connect-zones m zone1 zone2 1))
  ([m zone1 zone2 width]
   (->>$ (closest-pair zone1 zone2 5)
     ((fn [[a b]] (path-between a b width)))
     (remove-illegal-tiles m)
     (filter (gmap/intraversable-in?-fn m))
     (fill m $ :ground))))

(defn connect-zone-pairs
  "Connect the zones (a collection of zones) in m that is a pair in pairs."
  [m zones pairs]
  (let [zones-vec (vec zones)
        conns (m/emap zones-vec (vec pairs))]
    (reduce (fn [m [z1 z2]] (connect-zones m z1 z2))
            m
            conns)))

(defn connect-all-rooms
  "Connect the rooms in m so that every walkable tile in m is reachable from
   every other."
  ([m] (connect-all-rooms m (all-tiles m)))
  ([m zone]
   (let [{:keys [walls rooms]} (walls-and-rooms m zone)]
     (loop [room1 (first rooms) others (rest rooms) m m]
       (if (seq others)
         (let [room2 (first others)
               [a b] (closest-pair room1 room2)
               path (path-between a b)
               to-remove (filter (gmap/intraversable-in?-fn m) path)]
           (recur (math/union room1 room2 to-remove) (rest others)
                  (fill m to-remove :ground)))
         m)))))

(defn frame-tiles
  "Returns a set of tiles that makes up a rectangular frame from bottom
   (inclusive) to top (exclusive)."
  ([top] (frame-tiles [0 0] top))
  ([bottom top]
   (let [[bx by] bottom
         [w h] top]
     (math/union
      (area-between [bx by] [w (inc by)])
      (area-between [bx by] [(inc bx) h])
      (area-between [bx (dec h)] [w h])
      (area-between [(dec w) by] [w h])))))

(defn edge-zones
  "Returns the indicies of the zones that are on the edege of a map from
   [0 0] to map-shape."
  [zones map-shape]
  (let [border (frame-tiles map-shape)]
    (keep-indexed (fn [i z] (when (seq (math/intersection border z)) i))
                  zones)))

(defn wall-border [m zone]
  (fill m (inner-border zone)))
