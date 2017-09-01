(ns game.terrain-generator
  (:require
   [clojure.core.matrix :as m]
   [clojure.set :as set]
   [game.game-map :as gmap]
   [game.math :as math]
   [game.tile-sets :as ts]
   [game.utils :refer :all]))

(def extra-dist-between-points 1.05)

(defn ca-step
  "Cellular automaton step. Makes a tile ground, if at least
   limit tiles within distance dist is also ground."
  ([m dist limit]
   (ca-step m (ts/all-tiles m) dist limit))
  ([m zone dist limit]
   (let [get-vals (fn [m poses] (map #(get-in m %) poses))
         ps (ts/shrink-zone zone dist)]
     (->> ps
       (map #(count (filter #{:ground} (get-vals m (ts/all-neighbors % dist)))))
       (zip ps)
       (reduce (fn [m [pos v]]
                 (assoc-in m pos (if (>= v limit) :ground :wall)))
               m))))
  ([m zone dist limit steps]
   (call-times steps #(ca-step % zone dist limit) m)))

(defn fill-edge
  "Fills the edge of the given width of m with walls."
  [m width]
  (let [[x y] (math/mat-size m)]
    (ts/fill m (set/difference (ts/all-tiles m)
                               (ts/all-tiles m :indent width)))))

(defn ca-dungeon-terrain [m zone wall-ratio dist steps close-dist]
  (let [outer-border (ts/unsafe-outer-border zone)
        big-zone (ts/grow-zone zone dist)
        ;; If dist is more than 2, limit will be wrong.
        limit (if (= 1 dist) 5 13)]
    (-> m
      (ts/fill big-zone :ground)
      (ts/fill-randomly wall-ratio :area big-zone :value :wall)
      (ca-step big-zone dist limit steps)
      (ts/fill (math/difference big-zone zone) :wall)
      (ts/connect-close-rooms zone close-dist)
      (ts/connect-all-rooms zone))))

(defn randomly-scatter-terrain [m zone ratio thing]
  (-> m
    (ts/fill-randomly ratio :value thing :area zone)
    (ts/connect-all-rooms zone)))

(defn random-without-clumps
  "Randomly places things in zone until the traversable space is ratio things.
   Does not place more than limit things in next to another thing within
   radius."
  [m zone ratio limit radius thing]
  (let [ground (ts/traversable m zone)]
    (first
     (reduce
      (fn [[m thing-count] tile]
        (cond-pairs
          [(< ratio (/ thing-count (count ground)))
           (reduced [m])]
          [(<= (->$ (ts/points-in-circle tile radius)
                 (ts/filter-tiles m $ thing)
                 count)
               limit)
           [(assoc-in m tile thing) (inc thing-count)]]
          [:else [m thing-count]]))
      [m 0.0]
      (shuffle ground)))))

(defn create-random-terrain [m zone]
  (let [f rand-uniform
        i rand-uniform-int
        rst #(randomly-scatter-terrain
              %1 (ts/traversable %1 zone) %2 %3)]
    (cond-> m
      (chance 0.6) (ca-dungeon-terrain
                    zone (f 0.46 0.50) 1 (i 2 3) (f 5 7))
      true (rst (f 0.01 0.12) :tree)
      true (rst (f 0.01 0.12) :stone))))

(defn random-points-chain
  "Returns a seq of random points starting with [0 0], such that any two
   adjacent points are extra-dist-between-points apart, and no two points are
   closer together than 0.999 extra-dist-between-points. In other words, the
   points form a chain that does not loop back on itself."
  [n]
  (let [start [0 0]]
    (loop [ps [start] last-p start]
      (if (< (count ps) n)
        (let [v (math/random-angle)
              p (->> [(math/sin v) (math/cos v)]
                  (mapv #(* % extra-dist-between-points))
                  (mapv + last-p))]
          (if (every? #(> (math/distance p %)
                          (* 0.999 extra-dist-between-points))
                      ps)
            (recur (conj ps p) p)
            (recur [start] start)))
        ps))))

(defn translate-to-min-0
  "Move all points equally and minimally, so that no component of any point is
   negative."
  [points]
  (let [[bottom top] (ts/find-bounds points)]
    (m/add points (map - bottom))))

(defn add-intermediate-points
  "Adds the halfway points between the given points when viewed as a path."
  [points]
  (if (== 1 (count points))
    points
    (take (dec (* 2 (count points)))
          (interleave points
                      (map (fn [[p1 p2]] (mapv (comp #(/ % 2) +) p1 p2))
                           (pair-cycle points))))))

(defn select-close-point [m point]
  (first (sort-by #(math/distance % point)
                  (first (:rooms (ts/walls-and-rooms m))))))

(defn start-and-end [m points]
  (let [points (if (== 1 (count points))
                 (let [[x y] (math/mat-size m)]
                   (shuffle [[0 0] [x y] [0 y] [x 0]]))
                 points)
        close-to #(vec (select-close-point m %))]
    (zipmap [:start :end]
            (map close-to [(first points) (last points)]))))

(defn monster-spawns [m monsters]
  (take monsters (shuffle (remove (gmap/intraversable-in?-fn m)
                                  (ts/all-tiles m)))))

(defn make-round-rooms [num-points radius monsters ratio]
  (let [points (random-points-chain num-points)
        r+1 (+ radius 1)
        centers (->$ points
                  add-intermediate-points
                  translate-to-min-0
                  (m/mul (* 3 radius))
                  (m/emap math/round $)
                  (m/add [r+1 r+1]))
        [bottom top] (ts/find-bounds centers)
        ;; + 1 because if a point is at n, it's really betwen n and n + 1
        [x y] (map #(+ % r+1 1) top)
        m (ts/make-map x y :wall)
        m (reduce (fn [m* c] (ts/fill m* (ts/points-in-circle c radius)
                                      :ground))
                  m
                  centers)
        m (-> m
            (ts/fill-randomly ratio)
            (ca-step 1 5 2)
            ts/connect-all-rooms)]
    (merge {:terrain m :spawns (monster-spawns m monsters)}
           (start-and-end m centers))))

(defn expand-point
  "Makes a small image of a randomly generate area, by randomly expanding a
   point."
  [n]
  (let [size [20 20]
        p (mapv #(/ % 2) size)
        add (fn add [m [p & ps]]
              (if ps
                (assoc-in (add m ps) p :wall)
                (assoc-in m p :wall)))]
    (loop [m (assoc-in (apply ts/make-map size) p :wall)
           ps #{p}
           border (set (ts/cross-neighbors m p))
           i n]
      (if (<= i 0)
        #_(show-map m) nil
        (let [rps (take (* (count border) 0.7) (shuffle (seq border)))
              ps (into ps rps)]
          (recur (add m rps)
                 ps
                 (set/difference (into border (apply ts/cross-neighbors m rps))
                                 (set ps))
                 (dec i)))))))
