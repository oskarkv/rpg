(ns game.items
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.constants :as consts]
   [game.hierarchies :as hier]
   [game.math :as math]
   [game.stats :as stats]
   [game.utils :refer :all]))

(declare items)

(defn random-variables-with-mean
  "Returns num-vars random variables between 0 and 1, with average value avg."
  [avg num-vars]
  (when (pos? num-vars)
    (let [vars (repeatedly num-vars rand)
          vars-avg (/ (apply + vars) num-vars)
          adjusting-fn (fn [avg target]
                         (if (> avg target)
                           #(* (/ target avg) %)
                           #(+ % (* (- 1 %) (/ (- target avg) (- 1 avg))))))]
      (map (adjusting-fn vars-avg avg) vars))))

(defn rolls->stats
  "Takes a map of stats (from stat name to magnitude) and a set of rolls
   (numbers between 0 and 1) and modifies the stats of the map depending on the
   rolls and consts/stats-random-part."
  [base-stats rolls]
  (letfn [(rolls->stats-factors [vars]
            (map #(+ 1 (* consts/stats-random-part (- (* % 2) 1))) vars))]
    (into {} (map (fn [[stat magnitude] factor]
                    [stat (math/round (* factor magnitude))])
                  base-stats
                  (rolls->stats-factors rolls)))))

(defn roll-for-stats
  "Returns a new item with the stats randomly modified up or down a bit."
  [{id :id :as item}]
  (if-let [stats (get-in items [id :stats])]
    (let [rolls (random-variables-with-mean (rand) (count stats))
          actual-stats (rolls->stats stats rolls)]
      (assoc item :stats actual-stats))
    item))

(defn stack
  "Returns a vector of stack sizes that would result when trying to stack n
   items with a max stack size of max-stack-size."
  [n max-stack-size]
  (loop [left n stacks []]
    (if-not (pos? left)
      stacks
      (let [this-stack (min max-stack-size left)]
        (recur (- left this-stack) (conj stacks this-stack))))))

(defn unstack
  "Returns a seq of stacks of the given item. The seq will be longer than 1 if
   :quantity > :stackable."
  [{:keys [quantity id] :as item}]
  (let [allowed (or (get-in items [id :stackable]) 1)
        stacks (stack (or quantity 1) allowed)]
    (map (fn [n] (if (< 1 allowed)
                   (assoc item :quantity n)
                   (dissoc item :quantity)))
         stacks)))

(defn what-kind [thing]
  (condf thing
    hier/classes :classes
    hier/gear-slots :slots))

(defn make-stats-map [m]
  (let [{:keys [damage delay]} m
        stats (dissoc m :damage :delay)]
    (cond-> {:stats stats}
      damage (assoc :damage damage)
      delay (assoc :delay delay)
      (zero? (count stats)) (dissoc :stats))))

(defn all-info
  "Merges the full info of an item into a light version of it."
  [light-item]
  (merge (items (:id light-item)) light-item))

(defn concrete-slots [abstract-slots]
  (when abstract-slots
    (set (mapcat (fn [slot]
                   (if (hier/left-right-slots slot)
                     (hier/left-right slot)
                     [slot]))
                 abstract-slots))))

(defn item [name icon & info]
  (-> (apply merge-with set/union
             {:name name :icon icon}
             (for [e info]
               (condf e
                 map? (if (some #{:stackable :price} (keys e))
                        e
                        (make-stats-map e))
                 vector? {(what-kind (first e)) (set e)}
                 number? {:weight e}
                 hier/types {:type e}
                 keyword? {e true})))
    (update :slots concrete-slots)
    remove-map-nils))

(defn check-item [item]
  (let [{:keys [name slots type weight race class two-hand stackable]
         item-stats :stats}
        item]
    (every? identity
            [(string? name)
             (every? hier/classes class)
             (contains? (conj hier/types nil) type)
             (every? hier/gear-slots slots)
             (number? weight)
             (every? stats/all-stats (keys item-stats))
             (contains? #{true nil false} two-hand)
             (or (not stackable) (number? stackable))])))

(defn report-item [item]
  (if (check-item item)
    true
    (do (throw-error item " is not legal") false)))

(defn check-items [items]
  (when (and (every? report-item items)
             (reduce (fn [name-set name]
                       (if (name-set name)
                         (do (throw-error "Item " name " already exists")
                             (reduced false))
                         (conj name-set name)))
                     #{} (map :name items)))
    items))

(defn correct-slot? [item path]
  (if (nil? item)
    true
    (let [slot (peek path)
          item-type (items (:id item))]
      (if (contains? hier/gear-slots slot)
        (contains? (:slots item-type) slot)
        true))))

(defn get-tooltip [item]
  (let [{:keys [type damage delay stats] :as item}
        (all-info item)
        list-fn (fn [k] (when (k item)
                          (str (name k) ": "
                               (str/join ", " (map name (k item))))))]
    (->$ [(:name item)
          (when type (name type))
          (when damage (str "damage / delay: " damage " / " delay))]
      (into (for [[s v] stats]
              (str (name s) ": " v)))
      (into (keep list-fn [:classes :slots :races]))
      (str/join "\n" $))))

(def items
  (check-items
   [(item "Leather Vest" "leather_armor.png" 5 [:chest] :leather {:armor 20})
    (item "Snake Skin" "snakeskin.png" 0.1 {:stackable 20})
    (item "Rusty Sword" "longsword.png" 5 {:damage 3 :delay 20}
          [:main-hand :off-hand] [:paladin :warrior])]))
