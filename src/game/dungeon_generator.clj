(ns game.dungeon-generator
  (:require [game.math :as math]
            [mikera.image.core :as imz]
            [clojure.set :as set])
  (:use game.utils))

(def tile-types (zipmap [:wall :floor :monster :start :end] (range)))

(defn tile-type [x]
  (if (keyword? x) (x tile-types) x))

(def wall? zero?)

(def extra-dist-between-points 1.05)

(defn v+ [& vs] (apply mapv + vs))

(defn v- [& vs] (apply mapv - vs))

(defn v*s [v s] (mapv #(* % s) v))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn wall-in? [m]
  #(wall? (get-in m %)))

(defn make-map
  "Make an x by y map (matrix), initialized with the optional value v. If v is
   not provided, use the value for floor."
  ([x y] (make-map x y :floor))
  ([x y v]
   (vec (repeat x (vec (repeat y (tile-type v)))))))

(defn map-size [m]
  [(count m) (count (m 0))])

(defn line-crossed?
  "Returns true iff a line from p shooting straight up, infinitely, crosses
   line."
  [p line]
  (let [[px py] p
        [[l1x l1y] [l2x l2y]] (sort-by #(% 0) line)]
    (and (not (== l1x l2x))
         (<= l1x px) (< px l2x)
         (< py (+ l1y (* (/ (- l2y l1y) (- l2x l1x)) (- px l1x)))))))

(defn inside?
  "Returns true iff p is inside polygon (defined as a seq of points)."
  [p polygon]
  (->> (partition 2 1 (cycle polygon))
    (take (count polygon))
    (filter #(line-crossed? p %))
    count odd?))

(defn poses-between
  "Returns a set of the tile positions that exist in a rectangle defined by the
   2d points bottom and top, inclusive. Note that top should have a larger x
   and a larger y than bottom."
  [bottom top]
  (let [[xs ys] (map vector bottom (map inc top))]
    (set (for [y (apply range ys)
               x (apply range xs)]
           [x y]))))

(defn all-poses [m & {:keys [indent bottom top]
                      :or {indent 0 bottom [0 0] top (map dec (map-size m))}}]
  (let [bottom (map #(+ % indent) bottom)
        top (map #(- % indent) top)]
    (poses-between bottom top)))

(defn fill
  ([m poses] (fill m poses :wall))
  ([m poses v]
   (reduce (fn [m path] (assoc-in m path (tile-type v)))
           m poses)))

(defn fill-randomly
  "Fill ratio of m randomly with the value v (defaults to the wall value if
   not provided)."
  ([m ratio] (fill-randomly m ratio :wall))
  ([m ratio v]
   (let [[x y] (map-size m)
         number (math/floor (* ratio (* x y)))]
     (fill m (take number (shuffle (all-poses m))) v))))

(defn remove-illegal-poses
  "Returns only the positions of poses that are legal in m."
  [m poses]
  (let [[xs ys] (map-size m)]
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
            adj (remove (wall-in? m) (cross-neighbors m c))
            new-adj (remove v adj)]
        (recur (into (pop q) new-adj) (into v new-adj))))))

(defn walls-and-rooms
  "Returns a map of :walls and :rooms, where walls is the set of all wall
   tiles, and rooms is a vector of connected sets of room tiles in m."
  [m]
  (let [all (all-poses m)
        {walls true other false} (group-by (wall-in? m) all)]
    {:walls walls :rooms
     (loop [rooms [] left other]
       (if (seq left)
         (let [t (first left)
               room (flood-fill m t)]
           (recur (conj rooms room) (remove room left)))
         rooms))}))

(defn rectangle-between-points
  "Returns a rectangle (defined by 4 points) around p1 and p2, as if by first
   extending the line segment between p1 and p2 by extra on each end, and then
   widening the line."
  [p1 p2 width extra]
  (let [fw (math/norm-diff p2 p1)
        bw (v- fw)
        [e-> e<-] (map #(v*s % extra) [fw bw])
        [v1 v2] (map #(v*s % (/ width 2.0))
                     (let [[x y] fw] [[y (- x)] [(- y) x]]))]
    [(v+ p1 v1 e<-) (v+ p1 v2 e<-) (v+ p2 v2 e->) (v+ p2 v1 e->)]))

(defn ca-step
  "Cellular automaton step. Makes a tile a floor, if at least
   limit tiles within distance dist is also a floor."
  ([m dist limit]
   (let [get-vals (fn [m poses] (map #(get-in m %) poses))
         ps (all-poses m :indent dist)]
     (reduce (fn [m [pos v]]
               (assoc-in m pos (if (>= v limit) 1 0)))
             m
             (map vector
                  ps
                  (map #(reduce + (get-vals m (all-neighbors m dist %))) ps)))))
  ([m dist limit steps] (call-times steps #(ca-step % dist limit) m)))

(defn fill-edge
  "Fills the edge of width width of m with walls."
  [m width]
  (let [[x y] (map-size m)]
    (fill m (set/difference (all-poses m) (all-poses m :indent width)))))

(defn make-image [m]
  (let [{:keys [wall floor monster start end]} tile-types
        colors (fmap unchecked-int {wall 0xff000000 floor 0xffffffff
                                    monster 0xffff0000 start 0xff00ff00
                                    end 0xff00aaaa})
        zoom 6
        [xs ys] (map-size m)
        img (imz/new-image (* xs zoom) (* ys zoom))
        px (int-array (* xs ys zoom zoom) (unchecked-int 0xffff0000))]
    (dotimes* [x xs y ys]
      (let [color (int (colors (get-in m [x y])))]
        (dotimes* [i zoom j zoom]
          (aset px (+ (* (+ (* zoom y) i) xs zoom)
                      (+ (* zoom x) j))
                color))))
    (imz/set-pixels img px)
    img))

(defn show-map [m & {:keys [zoom]}]
  (imz/show (make-image m) :zoom (or zoom 1)))

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

(defn connect-rooms [m]
  (let [{:keys [walls rooms]} (walls-and-rooms m)]
    (if (> (count rooms) 1)
      (let [[a b] (closest-pair rooms)
            rect (rectangle-between-points a b 1.5 0.5)
            to-remove (filter #(inside? % rect) walls)
            new-m (fill m to-remove :floor)]
        (recur new-m))
      m)))

(defn random-angle []
  (rand-uniform 0 (* 2 math/pi)))

(defn random-points-chain
  "Returns a seq of random points starting with [0 0], such that any two
   adjacent points are extra-dist-between-points apart, and no two points are
   closer together than 0.999 extra-dist-between-points. In other words, the
   points form a chain that does not loop back on itself."
  [n]
  (let [start [0 0]]
    (loop [ps [start] last-p start]
      (if (< (count ps) n)
        (let [v (random-angle)
              p (->> [(math/sin v) (math/cos v)]
                  (mapv #(* % extra-dist-between-points))
                  (mapv + last-p))]
          (if (every? #(> (math/distance p %)
                          (* 0.999 extra-dist-between-points))
                      ps)
            (recur (conj ps p) p)
            (recur [start] start)))
        ps))))

(defn find-bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    {:maxx (apply max xs) :minx (apply min xs)
     :maxy (apply max ys) :miny (apply min ys)}))

(defn map-over-points [f points]
  (map (fn [p] (mapv f p)) points))

(defn multiply-points [points scalar]
  (map-over-points #(* scalar %) points))

(defn add-to-points [points [dx dy]]
  (map (fn [[x y]] [(+ x dx) (+ y dy)]) points))

(defn translate-to-min-0
  "Move all points equally and minimally, so that no component of any point is
   negative."
  [points]
  (let [{:keys [minx miny]} (find-bounds points)]
    (add-to-points points (map - [minx miny]))))

(defn points-in-circle [center radius]
  (let [top (map #(+ % radius) center)
        bottom (map #(- % radius) center)]
    (remove #(> (math/distance % center) radius)
            (poses-between bottom top))))

(defn add-intermediate-points [points]
  (if (== 1 (count points))
    points
    (take (dec (* 2 (count points)))
          (interleave points
                      (map (fn [[p1 p2]] (mapv (comp #(/ % 2) +) p1 p2))
                           (cycle (partition 2 1 points)))))))

(defn select-close-point [m point]
  (first (sort-by #(math/distance % point)
                  (first (:rooms (walls-and-rooms m))))))

(defn start-and-end [m points]
  (let [{:keys [start end]} tile-types
        points (if (== 1 (count points))
                 (let [[x y] (map-size m)]
                   (shuffle [[0 0] [x y] [0 y] [x 0]]))
                 points)
        close-to #(vec (select-close-point m %))]
    (zipmap [:start :end]
            (map close-to [(first points) (last points)]))))

(defn monster-spawns [m monsters]
  (take monsters (shuffle (remove (wall-in? m) (all-poses m)))))

(defn make-round-rooms [num-points radius monsters ratio]
  (let [points (random-points-chain num-points)
        r+1 (+ radius 1)
        centers (-> points
                  add-intermediate-points
                  translate-to-min-0
                  (multiply-points (* 3 radius))
                  (#(map-over-points math/round %))
                  (add-to-points [r+1 r+1]))
        {:keys [maxx maxy minx miny]} (find-bounds centers)
        ;; + 1 because if a point is at n, it's really betwen n and n + 1
        [x y] (map #(+ % r+1 1) [maxx maxy])
        m (make-map x y :wall)
        m (reduce (fn [m* c] (fill m* (points-in-circle c radius) :floor))
                  m
                  centers)
        m (-> m
            (fill-randomly ratio)
            (ca-step 1 5 2)
            connect-rooms)]
    (merge {:terrain m :spawns (monster-spawns m monsters)}
           (start-and-end m centers))))

(defn expand-point
  "Makes a small image of a randomly generate area, by randomly expanding a
   point."
  [n]
  (let [size [20 20]
        p (mapv #(/ % 2) size)
        wall (tile-type :wall)
        add (fn add [m [p & ps]]
              (if ps
                (assoc-in (add m ps) p wall)
                (assoc-in m p wall)))]
    (loop [m (assoc-in (apply make-map size) p wall)
           ps #{p}
           border (set (cross-neighbors m p))
           i n]
      (if (<= i 0)
        (show-map m)
        (let [rps (take (* (count border) 0.7) (shuffle (seq border)))
              ps (into ps rps)]
          (recur (add m rps)
                 ps
                 (set/difference (into border (apply cross-neighbors m rps))
                                 (set ps))
                 (dec i)))))))
