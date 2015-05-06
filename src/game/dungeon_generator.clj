(ns game.dungeon-generator
  (:require [clojure.math.numeric-tower :as math]
            [game.math :as gmath]
            [mikera.image.core :as imz])
  (:use game.utils
        game.java-math))


(def map-types (zipmap [:wall :floor :monster :start :end] (range)))

(def wall? zero?)

(def extra-dist-between-points 1.05)

(defn v+ [& vs] (apply mapv + vs))

(defn v- [& vs] (apply mapv - vs))

(defn v*s [v s] (mapv #(* % s) v))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn wall-in? [m]
  #(wall? (get-in m %)))

(defn make-map
  ([x y] (make-map x y (:floor map-types)))
  ([x y v]
   (vec (repeat x (vec (repeat y v))))))

(defn map-size [m]
  [(count m) (count (m 0))])

(defn line-crossed? [p line]
  (let [[px py] p
        [[l1x l1y] [l2x l2y]] (sort-by #(% 0) line)]
    (and (not (== l1x l2x))
         (<= l1x px) (< px l2x)
         (< py (+ l1y (* (/ (- l2y l1y) (- l2x l1x)) (- px l1x)))))))

(defn inside? [p polygon]
  (->> (partition 2 1 (cycle polygon))
       (take (count polygon))
       (filter #(line-crossed? p %))
       count odd?))

(defn poses-between [bottom top]
  (let [[xs ys] (map vector bottom (map inc top))]
    (for [y (apply range ys)
          x (apply range xs)]
      [x y])))

(defn all-poses [m & {:keys [indent bottom top]
                      :or {indent 0 bottom [0 0] top (map dec (map-size m))}}]
  (let [bottom (map #(+ % indent) bottom)
        top (map #(- % indent) top)]
    (poses-between bottom top)))

(defn fill
  ([m poses] (fill m poses (:wall map-types)))
  ([m poses v]
   (reduce (fn [m path] (assoc-in m path v))
           m poses)))

(defn fill-randomly [m percent]
  (let [[x y] (map-size m)
        ones (math/floor (* percent (* x y)))]
    (fill m (take ones (shuffle (all-poses m))))))

(defn remove-illegal-poses [m poses]
  (let [[xs ys] (map-size m)]
    (remove (fn [[x y]] (or (< x 0) (< y 0) (>= x xs) (>= y ys))) poses)))

(defn all-neighbors [m dist pos]
  (let [[x y] pos
        range* (range (- dist) (inc dist))]
    (remove-illegal-poses
      m (for [j range* i range*]
          [(+ x i) (+ y j)]))))

(defn cross-neighbors [m pos]
  (let [[x y] pos]
    (remove-illegal-poses
      m [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

;; only fills rooms
(defn flood-fill [m start-pos]
  (loop [q (conj empty-queue start-pos)
         v #{start-pos}]
    (if-not (peek q)
      v
      (let [c (peek q)
            adj (remove (wall-in? m) (cross-neighbors m c))
            new-adj (remove v adj)]
        (recur (into (pop q) new-adj) (into v new-adj))))))

(defn walls-and-rooms [m]
  (let [all (all-poses m)
        {walls true other false} (group-by (wall-in? m) all)]
    {:walls walls :rooms
     (loop [rooms [] left other]
       (if (seq left)
         (let [t (first left)
               room (flood-fill m t)]
           (recur (conj rooms room) (remove room left)))
         rooms))}))

(defn rectangle-between-points [p1 p2 width extra]
  (let [fw (gmath/norm-diff p2 p1)
        bw (v- fw)
        [e-> e<-] (map #(v*s % extra) [fw bw])
        [v1 v2] (map #(v*s % (/ width 2.0))
                     (let [[x y] fw] [[y (- x)] [(- y) x]]))]
    [(v+ p1 v1 e<-) (v+ p1 v2 e<-) (v+ p2 v2 e->) (v+ p2 v1 e->)]))

;; cellular automaton step
(defn ca-step [m dist limit steps]
  (let [get-values (fn [m poses] (map #(get-in m %) poses))]
    (if (pos? steps)
      (recur
        (reduce (fn [m [pos v]]
                  (assoc-in m pos (if (>= v limit) 1 0)))
                m
                (map vector
                     (all-poses m :indent dist)
                     (map #(apply + (get-values m (all-neighbors m dist %)))
                          (all-poses m :indent dist))))
        dist limit (dec steps))
      m)))

(defn fill-edge [m dist]
  (let [[x y] (map-size m)]
    (fill
      m (concat (for [i (range x) j (concat (range dist) (range (- y dist) y))]
                  [i j])
                (for [i (concat (range dist) (range (- x dist) x)) j (range y)]
                  [i j])))))

(defn make-image [m]
  (let [{:keys [wall floor monster start end]} map-types
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

(defn closest-pair [rooms]
  (let [[room & other] rooms
        others (apply concat other)
        take-some #(take (/ (count %) 2) %)
        [a b] (first (sort-by
                       #(% 2)
                       (for [a (take-some room) b (take-some others)]
                         [a b (gmath/distance a b)])))]
    [a b]))

;; flood fill would be better
(defn connect-rooms [m]
  (let [{:keys [walls rooms]} (walls-and-rooms m)]
    (if (> (count rooms) 1)
      (let [[a b] (closest-pair rooms)
            rect (rectangle-between-points a b 1.5 0.5)
            to-remove (filter #(inside? % rect) walls)
            new-m (fill m to-remove (:floor map-types))]
        (show-map new-m)
        (recur new-m))
      m)))

(defn random-angle []
  (rand-uniform 0 (* 2 pi)))

(defn random-points-chain [n]
  (let [start [0 0]]
    (loop [ps [[0 0]] last-p [0 0]]
      (if (< (count ps) n)
        (let [v (random-angle)
              p (->> [(sin v) (cos v)]
                     (mapv #(* % extra-dist-between-points))
                     (mapv + last-p))]
          (if (every? #(> (gmath/distance p %)
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

(defn map-to-point-components [f]
  (fn [points] (map (fn [p] (mapv f p)) points)))

(defn multiply-points [points scalar]
  ((map-to-point-components #(* scalar %)) points))

(defn add-to-points [points [dx dy]]
  (map (fn [[x y]] [(+ x dx) (+ y dy)]) points))

(defn translate-to-min-0 [points]
  (let [{:keys [maxx minx maxy miny]} (find-bounds points)
        [dx dy] (map #(- 0 %) [minx miny])]
    (add-to-points points [dx dy])))

(defn points-in-circle [center radius]
  (let [top (map #(+ % radius) center)
        bottom (map #(- % radius) center)]
    (remove #(> (gmath/distance % center) radius)
            (poses-between bottom top))))

(defn fill-circle [m center radius]
  (let [top (map #(+ % radius) center)
        bottom (map #(- % radius) center)]
    (fill m
          (remove #(> (gmath/distance % center) radius)
                  (all-poses m :top top :bottom bottom))
          0)))

(defn add-intermediate-points [points]
  (if (== 1 (count points))
    points
    (take (dec (* 2 (count points)))
          (interleave points
                      (cycle (map (fn [[p1 p2]] (mapv (comp #(/ % 2) +) p1 p2))
                                  (partition 2 1 points)))))))

(defn select-close-point [m point]
  (first (sort-by #(gmath/distance % point)
                  (first (:rooms (walls-and-rooms m))))))

(defn add-start-and-end [m points]
  (let [{:keys [start end]} map-types
        points (if (== 1 (count points))
                 (let [[x y] (map-size m)]
                   (shuffle [[0 0] [x y] [0 y] [x 0]]))
                 points)
        setter (fn [m p v] (assoc-in m (vec (select-close-point m p)) v))]
    (-> m (setter (first points) start) (setter (last points) end))))

(defn add-monsters [m monsters]
  (fill m (take monsters (shuffle (remove (wall-in? m) (all-poses m))))
        (:monster map-types)))

(defn make-round-rooms [num-points radius monsters percent]
  (let [points (random-points-chain num-points)
        r+1 (+ radius 1)
        centers (-> points
                    add-intermediate-points
                    translate-to-min-0
                    (multiply-points (* 3 radius))
                    ((map-to-point-components math/round))
                    (add-to-points [r+1 r+1]))
        {:keys [maxx maxy minx miny]} (find-bounds centers)
        ;; +1 because if a point is at n, the map needs n+1 cells
        [x y] (map #(+ % r+1 1) [maxx maxy])
        m (make-map x y (:wall map-types))
        m (reduce (fn [m* c] (fill m* (points-in-circle c radius)
                                   (:floor map-types)))
                  m centers)
        m (-> m
              (fill-randomly percent)
              (ca-step 1 5 2)
              connect-rooms
              (add-monsters monsters)
              (add-start-and-end centers))]
    m))
