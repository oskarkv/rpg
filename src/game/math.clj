(ns game.math
  (:require
   [clojure.core.matrix :as m]
   [clojure.math.numeric-tower]
   [clojure.set :as set]
   [game.utils :refer :all]))

(defn intern-from-ns [ns sym-list]
  (runmap (fn [sym]
            (intern *ns* sym (ns-resolve ns sym)))
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

(def tau (* 2 pi))

(def e Math/E)

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

(defn random-angle []
  (rand-uniform 0 tau))

(defn mat-size [m]
  [(count m) (count (m 0))])

(defn avg [& nums]
  (/ (apply + nums) (count nums)))

(defn normalize [v]
  (let [len (sqrt (apply + (map #(* % %) v)))]
    (mapv (if (zero? len) identity #(/ % len)) v)))

(defn norm-diff [v v2]
  (normalize (map - v v2)))

(defn squared-distance [[^double x ^double y] [^double x2 ^double y2]]
  (let [dx (- x2 x)
        dy (- y2 y)]
    (+ (* dx dx) (* dy dy))))

(defn distance [p p2]
  (sqrt (squared-distance p p2)))

(defn extrapolate-pos [pos dir time speed]
  (map + pos (map #(* speed time %) dir)))

(defn dot-product [v v2]
  (apply + (map * v v2)))

(defn angle-between [[x1 y1] [x2 y2]]
  (- (atan2 y2 x2) (atan2 y1 x1)))

(defn min-angle-between [v1 v2]
  (let [abv (abs (angle-between v1 v2))]
    (min abv
         (- tau abv))))

(defn rotate-vec [[x y] angle]
  [(- (* (cos angle) x) (* (sin angle) y))
   (+ (* (sin angle) x) (* (cos angle) y))])

(defn cross-product-2d [v1 v2]
  (let [[x1 y1] v1
        [x2 y2] v2]
    (- (* x1 y2) (* x2 y1))))

(defn midpoint [p1 p2]
  (mapv avg p1 p2))

(defn parallel [line line2]
  (let [v (apply map - line)
        v2 (apply map - line2)]
    (zero? (cross-product-2d v v2))))

(defn line-segment-intersection [[p1 p2] [q1 q2]]
  (when-lets
      [r (map - p2 p1)
       s (map - q2 q1)
       _ (not (zero? (cross-product-2d r s)))
       p-q (map - p1 q1)
       q-p (map - q1 p1)
       u (/ (cross-product-2d p-q r) (cross-product-2d s r))
       t (/ (cross-product-2d q-p s) (cross-product-2d r s))]
    (if (and (<= 0 u 1) (<= 0 t 1))
      (m/add p1 (m/mul r t)))))

(defn line-intersection
  "Returns the point of intersection between two lines defined by 2 points
   each, or nil if there is no intersection."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  ;; If lines are parallel, denom will be 0.
  (when-lets [denom (- (* (- x1 x2) (- y3 y4))
                       (* (- y1 y2) (- x3 x4)))
              _ (not (zero? denom))
              f12 (- (* x1 y2) (* y1 x2))
              f34 (- (* x3 y4) (* y3 x4))
              xnum (- (* f12 (- x3 x4))
                      (* (- x1 x2) f34))
              ynum (- (* f12 (- y3 y4))
                      (* (- y1 y2) f34))]
    [(/ xnum denom) (/ ynum denom)]))

(defn bisector
  "Returns a line, defined by two points, that is the perpendicular bisector of
   the line segment between p1 and p2."
  [p1 p2]
  (let [midpoint (midpoint p1 p2)
        [vx vy] (mapv - p1 p2)
        perp [(- vy) vx]]
    [(mapv - midpoint perp) (mapv + midpoint perp)]))

(defn solve-quad-poly
  "Solves 0 = ax2 + bx + c. Returns a sequence of solutions."
  [^double a ^double b ^double c]
  (let [root (sqrt (- (* b b) (* 4 a c)))
        x= #(/ (- % b) (* 2 a))]
    (if (zero? a)
      (if (zero? b)
        []
        [(- (/ c b))])
      (sort [(x= root) (x= (- root))]))))

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
  (->> (pair-cycle polygon)
    (filter #(line-crossed? p %))
    count odd?))

(defn difference [& colls]
  (apply set/difference (map set colls)))

(defn union [& colls]
  (apply set/union (map set colls)))

(defn intersection [& colls]
  (apply set/intersection (map set colls)))
