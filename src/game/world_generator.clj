(ns game.world-generator
  (:require
   [game.utils :refer :all]
   [loco.constraints :refer :all]
   [loco.core :as loco]
   [loom.alg :as lalg]
   [loom.attr :as lattr]
   [loom.graph :as loom]
   [loom.io :as lio]
   [loom.label :as llabel]))

(def graph {:a [:b :c]
            :c [:a :b :e :f]
            :d [:b :e :h]
            :e [:d :c :f :g]
            :f [:c :e :g]
            :g [:f :e]
            :b [:a :c :d :h]
            :h [:b :d]})

(def graph2 {:a [:b :c]
             :b [:c :d :h]
             :c [:e :f]
             :d [:e :h]
             :e [:f :g]
             :f [:g]
             :g [:e]
             :h []})

(def g (loom/graph graph))

(defn distance [g k k2]
  (dec (count (lalg/bf-path g k k2))))

(def model
  (let [gks (keys graph)
        n (count graph)]
    (concat
     [($cardinality gks {1 2, 2 2, 3 2, 4 2})]
     (for [k gks]
       ($in k 1 n))
     (for [k gks]
       ($if ($< k 4)
            (apply $or
                   (for [k2 (k graph)]
                     ($= k2 ($+ k 1))))))
     (for [k gks
           k2 gks
           :when (not= k k2)]
       ($if ($or ($= k k2 1) ($= k k2 2))
            ($> (distance g k k2) 2)))
     (for [k gks]
       ($not (apply $or
                    (for [k2 (k graph)]
                      ($= k k2))))))))

(defn add-labels [g node label & more]
  (let [ng (lattr/add-attr g node :label label)]
    (if (seq more)
      (apply add-labels ng more)
      ng)))

(defn testg []
  (let [sols (loco/solutions model)]
    (println (count sols))
    (dotimes [x (count sols)]
      (let [sol (nth sols x)
            g (apply add-labels (loom/graph graph)
                     (mapcat #(list %1 [%1 %2])
                             (keys graph)
                             (map sol (keys graph))))]
        (lio/view g)))))
