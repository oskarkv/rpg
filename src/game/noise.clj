(ns game.noise
  (:require
   [game.math :as math]
   [game.utils :refer :all]))

;; The number of different gradients, spread out on the unit circle, to use
(def different-gradients 8)

(def f-constant (-> 2 (+ 1) math/sqrt (- 1) (/ 2)))

(def g-constant (-> 1 (- (/ 1 (math/sqrt 3))) (/ 2)))

(def simplex-radius 0.55)

(def simplex-power 4)

(defn random-spread-unit-vectors [n]
  (let [slice (/ math/tau n)
        offset (rand slice)
        unit-vec [1 0]]
    (map #(math/rotate-vec unit-vec %)
         (take n (iterate #(+ % slice) offset)))))

(defn spread-vectors-seq [n]
  (let [vs (random-spread-unit-vectors n)]
    (apply concat (drop 1 (iterate shuffle vs)))))

(defn make-gradient-table []
  (let [s (atom (spread-vectors-seq different-gradients))]
    (memoize
     (fn [point]
       (swap! s next)
       (first @s)))))

(defn skew-transform-fn [f constant]
  (fn transform
    ([x y]
     (let [term (* (+ x y) constant)]
       [(f x term) (f y term)]))
    ([[x y]] (transform x y))))

(def skew (skew-transform-fn + f-constant))

(def unskew (skew-transform-fn - g-constant))

(defn skewed-base-and-internal [x y]
  (let [skewed (skew x y)
        base (mapv math/floor skewed)
        internal (mapv - skewed base)]
    [base internal]))

(defn get-3-corners [x y]
  (let [[[xb yb] [xi yi]] (skewed-base-and-internal x y)
        corners [[0 0] (if (> xi yi) [1 0] [0 1]) [1 1]]]
    (map #(map (comp int +) %1 %2) corners (cycle [[xb yb]]))))

(def scale-factor
  (let [triangle-side (math/distance [0 0] (unskew 0 1))
        to-center (* triangle-side (math/sqrt (/ 1 3)))
        max-dot (math/dot-product
                 (math/normalize [1 1]) [to-center to-center])]
    (/ (* 3 max-dot
          (math/expt (- simplex-radius (math/expt to-center 2))
                     simplex-power)))))

(defn simplex-noise-fn []
  (let [gradients-map (make-gradient-table)]
    (fn [x y]
      (let [corners (get-3-corners x y)
            gradients (map gradients-map corners)
            gradients-positions (map unskew corners)
            displacement-vs (map #(map - % [x y]) gradients-positions)
            dots (map math/dot-product gradients displacement-vs)]
        (->> (map (fn [disp dot]
                    (-> (max 0 (- simplex-radius
                                  (reduce + (map #(* % %) disp))))
                      (math/expt simplex-power)
                      (* dot)))
                  displacement-vs
                  dots)
          (reduce +)
          (* scale-factor)
          ;; 0.5 to and + 1 to make values go between 0 and 1, not -1 and 1
          (+ 1)
          (* 0.5))))))

(defn make-matrix
  "Make a matrix of size, using values from f. The inputs to f are distributed
   from 0 to input-max. Size and input-max can be a scalar or a vector of size
   2."
  [size input-max f]
  (let [[x-size y-size] (ensure-vec size)
        [x-max y-max] (ensure-vec input-max)
        scale-fn (fn [x scale size] (* scale (/ x size)))]
    (->> (for [x (map #(scale-fn % x-max x-size) (range x-size))]
           (for [y (map #(scale-fn % y-max y-size) (range y-size))]
             (f x y)))
      (vectorize))))

(defn fbm-fn [displacement period]
  (let [noisex (simplex-noise-fn)
        noisey (simplex-noise-fn)
        half-disp (/ displacement 2)]
    (fn [[x y]]
      (let [sx (/ x period)
            sy (/ y period)]
        [(int (+ x (- (* displacement (noisex sx sy)) half-disp)))
         (int (+ y (- (* displacement (noisey sx sy)) half-disp)))]))))
