(ns game.server.pathfinding
  (:require
   [clojure.data.priority-map :as pm]
   [game.constants :as consts]
   [game.game-map :as gmap]
   [game.math :as math]
   [game.utils :refer :all]))

(defn walkable? [x]
  (and x (gmap/walkable-type? x)))

(defn point->tile
  ([p] (apply point->tile p))
  ([x y] [(int x) (int y)]))

(defn get-tile
  "Gets the type of the tile at the point v in
   the grid m. v is a point in R^2, not grid indices."
  [m v]
  (get-in m (point->tile v)))

(defn integer-points
  "Given an equation: x = start + t * step, returns a list of the
   values for t that make x an integer between start and stop,
   or nil if there is no such value for t."
  [start stop step]
  (if-not (zero? step)
    (let [first-t (-> start ((if (neg? step) math/floor math/ceil))
                    (- start) (/ step))
          t-step (/ 1 (math/abs step))]
      (take-while #((if (neg? step) > <) (+ start (* step %)) stop)
                  (iterate (partial + t-step) first-t)))))

(defn crossed-tiles [[x y :as p] p2 m]
  (let [[dx dy :as diff-vec] (math/norm-diff p2 p)
        ipf (fn [getter]
              (integer-points (getter p) (getter p2) (getter diff-vec)))
        x-int-ps (ipf first)
        y-int-ps (ipf second)
        get-tile (fn [[x-indent y-indent] t]
                   (->> [(+ x-indent x (* t dx)) (+ y-indent y (* t dy))]
                     (get-tile m)))]
    (concat (map (partial get-tile [dx 0]) x-int-ps)
            (map (partial get-tile [0 dy]) y-int-ps))))

(defn clear-line?
  "Returns true if the line between p and p2 passes over only
   walkable? tiles in m, otherwise false."
  [p p2 m]
  (every? walkable? (crossed-tiles p p2 m)))

(defn clear-path?
  "Returns true if a circular object with radius r can move
   between p and p2, passing over only walkable? tiles in m,
   otherwise false.
   Note: Does not currently work for objects with a radius >= 0.5."
  [p p2 r m]
  (let [diff-vec (map (partial * r) (math/normalize (map - p2 p)))
        ortho1 ((fn [[x y]] (list (- y) x)) diff-vec)
        ortho2 ((fn [[x y]] (list y (- x))) diff-vec)
        s1 (map + ortho1 p) t1 (map + ortho1 p2)
        s2 (map + ortho2 p) t2 (map + ortho2 p2)]
    (and (clear-line? (map + ortho1 p) (map + ortho1 p2) m)
         (clear-line? (map + ortho2 p) (map + ortho2 p2) m))))

(defn straighten-path
  "Given a path in the map m, remove unnecessary nodes of
   the path. A node is removed if one can pass freely
   between the previous and the next node."
  ([m path radius]
   (if (> (count path) 2) (straighten-path m path radius nil) path))
  ([m [from mid to & tail] radius acc]
   (if to
     (if (clear-path? from to radius m)
       (recur m (list* from to tail) radius acc)
       (recur m (list* mid to tail) radius (conj acc from)))
     (reverse (conj acc from mid)))))

(defn to-mid-points [path]
  (map (partial map (partial + 0.5)) path))

(defn to-tiles [path]
  (map (partial map int) path))

(defn a*
  "A* search for a grid of squares, mat. Tries to find a
   path from start to goal using only walkable? tiles.
   start and goal are vectors of indices into the grid,
   not points in R^2."
  [mat start goal heuristic-factor]
  (let [width (count mat)
        height (count (first mat))]
    (letfn [(h [{pos :pos}] (* heuristic-factor (math/distance pos goal)))
            (g [{:keys [pos parent]}]
              (if parent
                (+ (:g parent) (math/distance pos (parent :pos)))
                0))
            (make-node [parent pos]
              (let [node {:pos pos :parent parent}
                    g (g node) h (h node)
                    f (+ g h)]
                (assoc node :f f :g g :h h)))
            (get-path
              ([node] (get-path node ()))
              ([{:keys [pos parent]} path]
               (if parent
                 (recur parent (conj path pos))
                 (conj path pos))))
            (free-tile? [tile]
              (let [type (get-in mat (vec tile))]
                (and type (walkable? type))))
            (expand [closed pos]
              (let [adj [[1 0] [0 1] [-1 0] [0 -1]]
                    add-pos (partial mapv + pos)]
                (->> (pair-cycle adj)
                  (map (fn [[t t2]]
                         (list* (map + t t2) (map add-pos [t t2]))))
                  (keep (fn [[d t t2]]
                          (if (every? free-tile? [t t2]) d nil)))
                  (concat adj)
                  (map add-pos)
                  (remove (fn [[x y :as tile]]
                            (or (closed tile) (neg? x) (neg? y)
                                (>= x width) (>= y height)
                                (not (walkable? (get-in mat tile)))))))))
            (add-to-open [open tile->node [{:keys [pos f] :as node} & more]]
              (if node
                (if (or (not (contains? open pos))
                        (< f (open pos)))
                  (recur (assoc open pos f)
                         (assoc tile->node pos node)
                         more)
                  (recur open tile->node more))
                {:open open :tile->node tile->node}))]
      (let [start-node (make-node nil start)]
        (loop [closed #{}
               open (pm/priority-map start (:f start-node))
               tile->node {start start-node}]
          (let [[curr _] (peek open) curr-node (tile->node curr)]
            (when curr
              (if (= curr goal)
                (get-path curr-node)
                (let [exp-tiles (expand closed curr)
                      exp-nodes (map (partial make-node curr-node) exp-tiles)
                      {:keys [open tile->node]}
                      (add-to-open (pop open) tile->node exp-nodes)]
                  (recur (conj closed curr) open tile->node))))))))))

(defn find-path [mat start goal radius]
  (if (clear-path? start goal radius mat)
    [goal]
    (when-let [path (a* mat (point->tile start) (point->tile goal)
                        consts/a*-heuristic-factor)]
      (let [point-path (to-mid-points path)
            full-path (concat [start] point-path [goal])
            final-path (rest (straighten-path mat full-path radius))]
        final-path))))
