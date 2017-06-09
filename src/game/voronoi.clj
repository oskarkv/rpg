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
  [n size]
  (let [find-r (fn [n size]
                 (-> (* size size)
                   (/ 4 n)
                   math/sqrt))
        r (find-r n size)
        k 10
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
                            (fn [[x y]] (and (<= 0 x size)
                                             (<= 0 y size)))
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
        first-sample [(rand size) (rand size)]]
    (loop [active #{first-sample}
           inactive []
           grid {(grid-pos first-sample) first-sample}]
      (if-let [p (first active)]
        (if-let [np (some #(test-point % grid) (gen-points p))]
          (recur (conj active np) inactive (conj grid [(grid-pos np) np]))
          (recur (disj active p) (conj inactive p) grid))
        inactive))))

;; We could break up the positions in a quad tree, and assign squares of
;; positions at once if all the corners have the same nearest site.
(defn warped-voronoi [sites [max-x max-y] displacement period]
  (let [fbm (noise/fbm-fn displacement period)]
    (group-by
     (fn [p] (let [p (fbm p)]
               (apply min-key #(math/squared-distance p %) sites)))
     (for [x (range max-x) y (range max-y)]
       [x y]))))

(defn voronoi [sites [max-x max-y]]
  (group-by
   (fn [p] (apply min-key #(math/squared-distance p %) sites))
   (for [x (range max-x) y (range max-y)]
     [x y])))

(defn to-color-map [zones size]
  (let [m (ts/make-map size size)]
    (reduce (fn [m points]
              (let [color (+ 0xff000000 (rand-int 0xffffff))]
                (reduce (fn [m [x y]]
                          (m/mset m x y color))
                        m
                        points)))
            m
            zones)))
