(ns game.math
  (:use game.java-math
        game.utils))

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
