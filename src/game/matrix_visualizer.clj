(ns game.matrix-visualizer
  (:require
   [clojure.core.matrix :as m]
   [game.math :as math]
   [game.utils :refer :all]
   [mikera.image.core :as imz])
  (:import
   (java.awt Color Font)))

;;; These color functions are very slow. show-image is ok.

(def color-map
  (fmap
   #(bit-or 0xff000000 %)
   {:white   0xffffff
    :black   0x000000
    :red     0xff0000
    :green   0x00ff00
    :blue    0x0000ff
    :yellow  0xffff00
    :teal    0x00ffff
    :magenta 0xff00ff}))

(defn to-color [color]
  (Color. (unchecked-int (if (keyword? color) (color color-map) color)) true))

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

(defn grayscale-matrix
  "Creates a matrix of color values from white to black, from a matrix of values
   between -1 and 1, or 0 and 1."
  [matrix]
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

(defn create-image [m]
   (let [[x-size y-size :as size] (math/mat-size m)
         img (apply imz/new-image size)]
     (dotimes* [x x-size y y-size]
       (imz/set-pixel img x (- (dec y-size) y) (get-in m [x y])))
     img))

(defn show-image
  "Displays an image (matrix of color values), typically not just a matrix
   straight from the game."
  ([img] (show-image img "show-image"))
  ([img title]
   (show-image img title 3))
  ([img title zoom]
   (imz/show img :resize zoom :title title)
   img))

(defn draw-string [img string [x y] color]
  (let [x (- x (int (math/ceil (* 2.5 (count string)))))
        y (- (.getHeight img) y -3)]
    (doto (.getGraphics img)
      (.setColor (to-color color))
      (.setFont (Font. "Ubuntu Mono Regular" 1 10))
      (.drawString string x y))
    img))
