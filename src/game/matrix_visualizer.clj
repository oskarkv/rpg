(ns game.matrix-visualizer
  (:require
   [clojure.core.matrix :as m]
   [game.math :as math]
   [game.utils :refer :all]
   [mikera.image.core :as imz]))

;;; These color functions are very slow. show-image is ok.

(def color-map
  {:white   [1 1 1]
   :black   [0 0 0]
   :red     [1 0 0]
   :green   [0 1 0]
   :blue    [0 0 1]
   :yellow  [1 1 0]
   :teal    [0 1 1]
   :magenta [1 0 1]})

(defn get-value-type [matrix]
  (let [cell (get-in matrix [0 0])]
    (if (keyword? cell)
      :keyword
      (let [emax (m/emax matrix)
            emin (m/emin matrix)]
        [(if (m/array? cell) true false)
         (cond (<= 0 emin emax 1) :zero-to-one
               (<= -1 emin emax 1) :minus-one-to-one
               :else :byte)]))))

(defn to-byte [x value-range]
  (condp = value-range
    :zero-to-one (int (* x 255))
    :minus-one-to-one (int (* (/ (inc x) 2) 255))
    :byte x))

(defn color-int [[r g b]]
  (+ (unchecked-int 0xff000000)
     (bit-shift-left r 16)
     (bit-shift-left g 8)
     b))

(defn gray-int [^long v]
  (+ (unchecked-int 0xff000000)
     (bit-shift-left v 16)
     (bit-shift-left v 8)
     v))

(defmulti to-color (fn [x value-type] value-type))

(defmethod to-color :keyword [v _]
  (color-int (map #(to-byte % :zero-to-one) (color-map v))))

(defmethod to-color :default [v [color? value-range]]
  (let [to-color* #(to-byte % value-range)]
    (color-int
     (if color?
       (map to-color* v)
       (repeat 3 (to-color* v))))))

(defn grayscale-matrix [matrix]
  (let [negatives (< (m/emin matrix) 0)
        gray (fn [v]
               (-> (if negatives
                     (* 127 (+ 1 v))
                     (* v 255))
                 unchecked-int
                 gray-int))]
    (m/emap gray matrix)))

(defn draw-line [m [p p2]]
  (let [v (math/norm-diff p2 p)
        d (int (math/distance p p2))]
    (reduce (fn [m p]
              (assoc-in m p 0xffff0000))
            m
            (m/emap int (mapv #(m/add p (m/mul v %)) (range (inc d)))))))

(defn show-image
  ([m]
   (show-image m 3))
  ([m zoom]
   (let [[x-size y-size :as size] (math/mat-size m)
         img (apply imz/new-image size)]
     (dotimes* [x x-size y y-size]
       (imz/set-pixel img x (- (dec y-size) y) (get-in m [x y])))
     ;;(imz/set-pixels img (int-array (m/eseq colors)))
     (imz/show (imz/scale img zoom)))))
