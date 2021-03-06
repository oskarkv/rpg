(ns game.voronoi
  (:require
   [clojure.core.matrix :as m]
   [game.math :as math]
   [game.noise :as noise]
   [game.tile-sets :as ts]
   [game.utils :refer :all]))

(defn random-points
  "Finds about 2.5 * n random but evently distributed points in the square from
   [0 0] to [size size], by the Poisson disc sampling algorithm."
  [n [width height]]
  (let [r (-> (* width height)
            (/ 4 n)
            math/sqrt)
        k 20
        ;; A grid of r/sqrt(2) cell length is used as a "background"
        ;; to speed up distance checking.
        cell-side (/ r (math/sqrt 2))
        grid {}
        grid-pos (fn [p] (mapv #(long (/ % cell-side)) p))
        bad-cells (for [x [2 -2] y [2 -2]] [x y])
        get-cells (fn [cell] (map #(m/add cell %)
                                  (for [x (range -2 3) y (range -2 3)
                                        :when (not (some #{[x y]} bad-cells))]
                                    [x y])))
        ;; Generate k random points distance r to 2r from p.
        gen-points (fn [p]
                     (take k
                           (filter
                            (fn [[x y]] (and (< 0 x width)
                                             (< 0 y height)))
                            (map #(m/add p %)
                                 (repeatedly
                                  #(math/rotate-vec [(+ r (rand r)) 0]
                                                    (rand math/tau)))))))
        ;; Test if p is not too close to another point.
        test-point (fn [p grid]
                     (if (some #(< (math/distance p %) r)
                               (keep grid (get-cells (grid-pos p))))
                       nil
                       p))
        first-sample [(rand width) (rand height)]]
    (loop [active #{first-sample}
           inactive []
           grid {(grid-pos first-sample) first-sample}]
      (if-let [p (first active)]
        (if-let [np (some #(test-point % grid) (gen-points p))]
          (recur (conj active np) inactive (conj grid [(grid-pos np) np]))
          (recur (disj active p) (conj inactive p) grid))
        inactive))))

(defn tiles-in-rect
  "Returns a seq of tiles in the rectangle from (0, 0) to (width, height)."
  [[width height]]
  (for [x (range width) y (range height)]
    [x y]))

;; We could break up the positions in a quad tree, and assign squares of
;; positions at once if all the corners have the same nearest site.
(defn warped-voronoi
  "Returns a map of sites to tiles, representing a voronoi diagram warped with a
   simplex noise function. The argument sites should be a seq of points, and
   shape should be [width height]."
  [sites shape displacement period]
  (let [fbm (noise/fbm-fn displacement period)]
    (group-by
     (fn [p] (let [p (fbm p)]
               (apply min-key #(math/squared-distance p %) sites)))
     (tiles-in-rect shape))))

(defn voronoi
  "Returns a map of sites to tiles, representing a voronoi diagram. The argument
   sites should be a seq of points, and shape should be [width height]."
  [sites shape]
  (group-by
   (fn [p] (apply min-key #(math/squared-distance p %) sites))
   (tiles-in-rect shape)))

(defn to-color-map [zones size]
  (let [m (ts/make-mat size size)]
    (reduce (fn [m points]
              (let [color (+ 0xff000000 (rand-int 0xffffff))]
                (reduce (fn [m [x y]]
                          (m/mset m x y color))
                        m
                        points)))
            m
            zones)))
