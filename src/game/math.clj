(ns game.math
  (:require [clojure.math.numeric-tower])
  (:use game.utils))

(defn intern-from-ns [ns sym-list]
  (runmap (fn [sym]
            (intern *ns* sym (ns-resolve *ns* (symbol (str ns) (str sym)))))
          sym-list))

(intern-from-ns
 'clojure.math.numeric-tower
 '(gcd
   floor
   ceil
   expt
   integer-length
   round
   lcm))

(def pi Math/PI)

(def sin #(Math/sin %))

(def cos #(Math/cos %))

(def sqrt #(Math/sqrt %))

(def atan2 #(Math/atan2 %1 %2))

(def abs #(Math/abs %))

(def exp #(Math/exp %))

(defn gaussian-fn [a x0 y0 sx sy]
  (fn gauss
    ([[x y]] (gauss x y))
    ([x y]
     (let [inner (fn [v v0 s]
                   (/ (expt (- v v0) 2)
                      (* 2 (expt s 2))))]
       (* a (exp (- (+ (inner x x0 sx) (inner y y0 sy)))))))))

(defn avg [& nums]
  (/ (apply + nums) (count nums)))

(defn normalize [v]
  (let [len (sqrt (apply + (map #(* % %) v)))]
    (map (if (zero? len) identity #(/ % len)) v)))

(defn norm-diff [v v2]
  (normalize (map - v v2)))

(defn distance [p p2]
  (sqrt (apply + (map #(* % %) (map - p p2)))))

(defn extrapolate-pos [pos dir time speed]
  (map + pos (map #(* speed time %) dir)))

(defn dot-product [v v2]
  (apply + (map * v v2)))

(defn angle-between [[x1 y1] [x2 y2]]
  (- (atan2 y2 x2) (atan2 y1 x1)))

(defn min-angle-between [v1 v2]
  (let [abv (abs (angle-between v1 v2))]
    (min abv
         (- (* 2 pi) abv))))

(defn rotate-vec [[x y] angle]
  [(- (* (cos angle) x) (* (sin angle) y))
   (+ (* (sin angle) x) (* (cos angle) y))])

(defn cross-product-2d [v1 v2]
  (let [[x1 y1] v1
        [x2 y2] v2]
    (- (* x1 y2) (* x2 y1))))
