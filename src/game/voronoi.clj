(ns game.voronoi
  (:require [clojure.math.numeric-tower :as math]
            [game.math :as gmath]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb])
  (:use game.utils
        game.java-math))

;;; This file contains code to compute voronoi diagrams using Fortune's
;;; algorithm. It is not quite complete. The basic sweep line part is done, but
;;; not the preparation of the diagram in a convinient form.
;;;
;;; The function fortunes returns a map of :edges and :vertices. The edges are
;;; pairs of sites which share a border between them. The vertices are triples
;;; of sites that lie on a circle, whose mid point is a vertex in the voronoi
;;; diagram, and the positon of the mid point.
;;;
;;; The following remains to be done:
;;; 1) Calcuate the actual edges between sites.
;;; 2) Incorporate a bounding frame around the sites into the diagram.
;;; 3) Choose an appropriate datastructure for the diagram. For example, one
;;;    might want to be able to answer queries about what the neighbors of a
;;;    site are.

(defn line-intersection
  "Returns the point of intersection between two lines defined by 2 points
   each. Will divide by zero if lines are parallel."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  ;; If lines are parallel, denom will be 0.
  (let [denom (- (* (- x1 x2) (- y3 y4))
                 (* (- y1 y2) (- x3 x4)))
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
  (let [midpoint (mapv gmath/avg p1 p2)
        [vx vy] (mapv - p1 p2)
        perp [(- vy) vx]]
    [(mapv - midpoint perp) (mapv + midpoint perp)]))

(defn solve-quad-poly
  "Solves 0 = ax2 + bx + c. Returns a sequence of solutions."
  [^double a ^double b ^double c]
  (let [root (math/sqrt (- (* b b) (* 4 a c)))
        x= #(/ (- % b) (* 2 a))]
    (if (zero? a)
      (if (zero? b)
        []
        [(- (/ c b))])
      (sort [(x= root) (x= (- root))]))))

(defn x-of-breakpoint
  "Calculates where the parabolas intersect. ly is the y of the sweep line."
  [[^double px ^double py] [^double qx ^double qy] ^double ly]
  (let [py (- py ly)
        qy (- qy ly)
        a (- qy py)
        b (- (* 2 qx py) (* 2 px qy))
        c (- (* qy (+ (* px px) (* py py)))
             (* py (+ (* qx qx) (* qy qy))))
        [x1 x2] (solve-quad-poly a b c)]
    (if (>= py qy) x1 x2)))

(defprotocol MutableNodeProtocol
  (get-left [this])
  (get-right [this])
  (set-left! [this leaf])
  (set-right! [this leaf])
  (get-parent [this])
  (set-parent! [this p]))

(defprotocol MutableLeafProtocol
  (get-site [this])
  (get-event [this])
  (set-event! [this e])
  (clone [this]))

(defprotocol MutableInternalNodeProtocol
  (get-left-leaf [this])
  (get-right-leaf [this])
  (set-left-leaf! [this s])
  (set-right-leaf! [this s]))

(deftype MutableLeaf [site
                      ^:unsynchronized-mutable parent
                      ^:unsynchronized-mutable left-leaf
                      ^:unsynchronized-mutable right-leaf
                      ^:unsynchronized-mutable event]
  MutableLeafProtocol
  (get-site [this]
    site)
  (get-event [this]
    event)
  (set-event! [this e]
    (set! event e)
    this)
  (clone [this]
    (new MutableLeaf site parent left-leaf right-leaf event))
  MutableNodeProtocol
  (get-left [this]
    left-leaf)
  (get-right [this]
    right-leaf)
  (set-left! [this leaf]
    (set! left-leaf leaf)
    this)
  (set-right! [this leaf]
    (set! right-leaf leaf)
    this)
  (get-parent [this]
    parent)
  (set-parent! [this p]
    (set! parent p)
    this)
  Object
  (toString [this]
    (str (some->> this
                  get-left
                  get-site
                  (mapv int))
         "<" (mapv int site) ">"
         (some->> this
                  get-right
                  get-site
                  (mapv int)))))

(deftype MutableNode [^:unsynchronized-mutable left-leaf
                      ^:unsynchronized-mutable right-leaf
                      ^:unsynchronized-mutable left-child
                      ^:unsynchronized-mutable right-child
                      ^:unsynchronized-mutable parent]
  MutableInternalNodeProtocol
  (get-left-leaf [this]
    left-leaf)
  (get-right-leaf [this]
    right-leaf)
  (set-left-leaf! [this s]
    (set! left-leaf s)
    this)
  (set-right-leaf! [this s]
    (set! right-leaf s)
    this)
  MutableNodeProtocol
  (get-left [this]
    left-child)
  (get-right [this]
    right-child)
  (set-left! [this child]
    (set! left-child child)
    this)
  (set-right! [this child]
    (set! right-child child)
    this)
  (get-parent [this]
    parent)
  (set-parent! [this p]
    (set! parent p)
    this)
  Object
  (toString [this]
    (str "[" (mapv int (get-site left-leaf)) " "
         (mapv int (get-site right-leaf)) "]")))

(defn tree-insert [tree new-leaf]
  (let [pos (get-site new-leaf)]
    (cond
      (nil? tree) new-leaf
      (instance? MutableLeaf tree)
      (let [old-right (get-right tree)
            new-left tree
            new-right (clone tree)
            first-node (->MutableNode new-left new-leaf
                                      new-left nil (get-parent tree))
            second-node (->MutableNode new-leaf new-right
                                       new-leaf new-right first-node)]
        (some-> old-right (set-left! new-right))
        (set-left! new-leaf new-left)
        (set-right! new-leaf new-right)
        (set-left! new-right new-leaf)
        (set-right! new-left new-leaf)
        (set-right! first-node second-node)
        (set-parent! new-left first-node)
        (set-parent! new-leaf second-node)
        (set-parent! new-right second-node)
        first-node)
      ;; Internal node
      :else
      (let [a (get-site (get-left-leaf tree))
            b (get-site (get-right-leaf tree))
            x (x-of-breakpoint a b (pos 1))
            [set-fn get-fn] (if (< (pos 0) x)
                              [set-left! get-left]
                              [set-right! get-right])]
        (set-fn tree (tree-insert (get-fn tree) new-leaf))))))

(defn tree-remove [tree leaf]
  (let [left (get-left leaf)
        right (get-right leaf)
        p (get-parent leaf)
        left? (= (get-left p) leaf)
        other-child ((if left? get-right get-left) p)
        pp (get-parent p)
        set-fn (if (= (get-left pp) p) set-left! set-right!)
        sites (map get-site
                   (if left? [left leaf] [leaf right]))]
    ;; Search for the internal node that represents the beach line
    ;; break point that needs updating.
    (loop [above pp]
      (if (= (map get-site ((juxt get-left-leaf get-right-leaf) above))
             sites)
        (if left?
          (set-right-leaf! above right)
          (set-left-leaf! above left))
        (recur (get-parent above))))
    ;; The left and right sites can't be the same site, because then
    ;; the middle arc couldn't have disappeared in a circle event
    (set-fn pp other-child)
    (set-parent! other-child pp)
    (set-left! right left)
    (set-right! left right)
    tree))

(defn lines-converge?
  "Returns true iff the vector from p1 to p2, followed by the vector from p2 to
   p3 makes a right turn. If so, the bisectors between the points converge on
   the right side."
  [p1 p2 p3]
  (let [[v2 v1] (map #(apply map - %) (partition 2 1 [p3 p2 p1]))]
    (neg? (gmath/cross-product-2d v1 v2))))

;; Kommer det att funka att ha en mutable sak i mappen?
(defn make-circle-event [[p1 p2 p3] leaf]
  (when (lines-converge? p1 p2 p3)
    (let [b1 (bisector p1 p2)
          b2 (bisector p2 p3)
          vertex (line-intersection b1 b2)
          [x y] vertex
          event-pos [x (- y (gmath/distance vertex p1))]]
      {:type :circle :vertex vertex :pos event-pos :leaf leaf})))

(defn get-triple [leaf]
  (let [chain [(get-left leaf) leaf (get-right leaf)]]
    (if (== (count (remove nil? chain)) 3)
      (map get-site chain)
      nil)))

(defn circle-events
  "Make circle events for the two triples where leaf is the left-most and
   right-most node, and set the event field of the appropriate leafs in the
   tree."
  [leaf]
  (let [make-event (fn [get-fn]
                     (let [mid-leaf (get-fn leaf)
                           ce (some-> (get-triple mid-leaf)
                                      (make-circle-event mid-leaf))]
                       (set-event! mid-leaf ce)
                       ce))
        left (make-event get-left)
        right (make-event get-right)]
    (map #(vector % (:pos %)) (remove nil? [left right]))))

(defn handle-site-event [{:keys [event tree diagram queue valid-events] :as m}]
  (let [pos (:pos event)
        new-leaf (->MutableLeaf pos nil nil nil nil)
        new-tree (tree-insert tree new-leaf)
        obsolete-event (some-> (get-left new-leaf) get-event)]
    (if (nil? tree)
      (assoc m :tree new-tree)
      (let [events (circle-events new-leaf)]
        (-> m
          (update :queue dissoc obsolete-event)
          (update-in [:diagram :edges]
                     conj #{pos (get-site (get-left new-leaf))})
          (update-in [:queue] into events)
          (assoc :tree new-tree))))))

(defn get-successors [tree]
  (if (instance? MutableNode tree)
    ((juxt get-left get-right) tree)
    []))

(defn print-tree [tree]
  (loop [parts [tree]]
    (when-not (empty? parts)
      (recur (mapcat get-successors parts)))))

(defn handle-circle-event
  [{:keys [event tree diagram queue valid-events] :as m}]
  (let [{:keys [vertex pos leaf]} event
        left (get-left leaf)
        left-site (get-site left)
        right (get-right leaf)
        right-site (get-site right)
        site (get-site leaf)
        old-ces (remove nil? (map get-event [left right]))
        _ (tree-remove tree leaf)
        new-ces (circle-events leaf)
        graph-vertex-map {:sites
                          (hash-set right-site left-site site)
                          :pos vertex}]
    (-> m
      (assoc :queue (apply dissoc queue old-ces))
      (update-in [:diagram :vertices] conj graph-vertex-map)
      (update-in [:diagram :edges] conj #{right-site left-site})
      (update-in [:queue] into new-ces))))

(defn fortunes [sites]
  (loop [diagram {}
         ;; Largest y first
         queue (into (pm/priority-map-by
                       (fn [[x y] [x2 y2]]
                         (if (= y y2) (< x x2) (> y y2))))
                     (map #(vector {:type :site :pos %} %) sites))
         tree nil
         valid-events #{}]
    (if (empty? queue)
      diagram
      (let [event (first (peek queue))
            {:keys [tree diagram valid-events queue]}
            ((if (= (:type event) :site)
               handle-site-event
               handle-circle-event)
             {:event event :tree tree :diagram diagram
              :queue (pop queue) :valid-events valid-events})]
        (recur diagram queue tree valid-events)))))

(defn build-voronoi-diagram
  "Takes a map with a list of edges (sets of two sites, the edge being their
   bisector) and a list of vertexes (sets of three points and a position), and
   transforms the map to a map from edges (sets of pairs of sites) to
   endpoints. Note that an edge can have 0, 1 or 2 endpoints, and if it has two,
   the two might be the same endpoint."
  [{:keys [edges vertices]}]
  (let [edges-map (into {} (for [s edges] [s nil]))]
    (->> (mapcat (fn [{:keys [sites pos]}]
                   (for [c (comb/combinations sites 2)]
                     [(set c) pos]))
                 vertices)
      (reduce (fn [m [s p]] (update m s conj p)) edges-map))))

(defn test-fortune []
  (fortunes (mapv #(mapv double %) [[1 1] [2 1] [1 2] [2 2] [1 3]])))

(defn test-fortune* []
  (build-voronoi-diagram
    (fortunes (mapv #(mapv double %) [[1 1] [2 1] [1 2] [2 2] [1 3]]))))
