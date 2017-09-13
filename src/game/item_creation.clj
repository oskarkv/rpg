(ns game.item-creation
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [game.constants :as consts]
   [game.hierarchies :as hier]
   [game.math :as math]
   [game.stats :as stats]
   [game.utils :refer :all]))

(defn normalize-map [m]
  (let [total (apply + (vals m))]
    (fmap #(/ % total) m)))

(defn scale-map [m scale]
  (fmap #(* scale %) m))

(defn armor? [type]
  (#{:cloth :leather :mail :plate} type))

(defn armor-factor-to-part
  "Takes a stats distribution map and an armor factor, and adds an armor part to
   the stats map based on the armor factor. The armor factor is how much of the
   standard amount of armor the item should have."
  [stats armor-factor]
  (let [armor-part (* stats/armor-ratio armor-factor)]
    (-> stats
      normalize-map
      (scale-map (- 1 armor-part))
      (assoc :armor armor-part))))

(defn pick-stats-randomly
  "Pick n stats out of the stats-map. If a key in stats-map has a higher value,
   it is more likely to be picked."
  [stats-map n]
  (take n (distinct (repeatedly #(random-pick stats-map)))))

(defn value-from-range
  "If value-range is a scalar, return it. Else pick a number in the range. If
   the range consists of integers, the return value will be an integer."
  [value-range]
  (if (sequential? value-range)
    (apply (if (every? int? value-range) rand-uniform-int rand-uniform)
           value-range)
    value-range))

;; Can be made to work with different-valued items by just picking more parts.
;; Not currently used.
(defn spread-stats-randomly
  "Spread the stats in stats-map out over num-items items so that the sum of the
   stats for the items have the same distribution as stats-map."
  [stats-map num-items]
  (let [parts-per-item 10
        stats-sum (apply + (vals stats-map))
        parts (mapcat (fn [[stat n]]
                        (repeat (* n num-items parts-per-item) stat))
                      stats-map)
        items (partition (int (* parts-per-item stats-sum)) (shuffle parts))]
    (map normalize-map
         (map (fn [parts]
                (fmap #(/ (float (count %)) parts-per-item)
                      (group-by identity parts)))
              items))))

(defn real-item-stats
  "Creates a map of stats for an item by taking into consideration all the
   relevant information from item-map. The given quality is used, and not the
   quality in the item-map."
  [item-map quality]
  (let [{:keys [armor stats num-stats level slot]} item-map
        n (value-from-range num-stats)
        stats-factor (* quality level
                        stats/stats-per-slot-per-level
                        (stats/relative-gear-slot-value slot))]
    (->$ stats
      (pick-stats-randomly n)
      (zipmap (repeat 1))
      (armor-factor-to-part (value-from-range armor))
      (fmap #(* stats-factor %) $)
      (update :armor *some (stats/armor-factor type)))))

(defn random-variables-with-mean
  "Returns num-vars random variables between 0 and 1, with mean value avg."
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
  (->> rolls
    (map #(inc (* consts/stats-random-part (dec (* % 2)))))
    (map (fn [[stat magnitude] factor] [stat (* factor magnitude)])
         base-stats)
    (into {})))

(defn roll-for-stats
  "Returns a new stats map with the stats randomly modified up or down a bit."
  [stats]
  (rolls->stats stats (random-variables-with-mean (rand) (count stats))))

(defn create-actual-item
  "Make an item by taking an item-map, which is almost an item, and calculates
   the actual stats, adds damage for weapons, and adjusts quality using
   extra-rarity."
  [item-map]
  (let [{:keys [quality extra-rarity delay two-hand level]} item-map
        stats-quality (* (if two-hand stats/relative-two-hander-value 1)
                         quality)]
    (cond-> item-map
      true (assoc :quality (+some quality extra-rarity)
                  :stats (roll-for-stats
                          (real-item-stats item-map stats-quality)))
      true (update :stats #(fmap math/round %))
      true (dissoc :extra-rarity :armor :num-stats)
      delay (assoc :damage (* stats-quality delay (stats/weapon-dps level))))))

(defn join-strs [& strs]
  (str/join " " (remove nil? strs)))

(defn handle-name [item-map]
  (let [{:keys [name-prefix name name-suffix]} item-map]
    (-> item-map
      (assoc :name (join-strs name-prefix name name-suffix))
      (dissoc :name-prefix :name-suffix))))

(defn expand-item-set
  "Takes a map that represents an item set, and creates the individual items in
   the set. If m is an individual item already, returns m as is."
  [m]
  (if-not (:instances m)
    m
    (map
     (fn [item-map]
       (-> (merge m item-map)
         (dissoc :instances)
         handle-name))
     (:instances m))))

(defn ns-qualify [k]
  (keyword (name (.name *ns*)) (name k)))

(defmacro def-many [ks spec]
  (let [ks @(ns-resolve *ns* ks)]
    `(do ~@(map
            #(list `s/def (ns-qualify %) spec)
            ks))))

(def-many stats/base-stats int?)

(s/def ::slot (set (map ns-qualify hier/gear-slots)))
(s/def ::type keyword?)
(s/def ::stats map?)
(s/def ::level int?)
(s/def ::quality number?)
(s/def ::name string?)
(s/def ::delay number?)
(s/def ::damage int?)
(s/def ::two-hand (s/or :nil nil? :bool boolean?))
(s/def ::item (s/keys :req [::level ::name ::slot ::type ::stats ::quality]
                      :opt [::damage ::delay ::two-hand]))

(defn check-items [items]
  (let [qitems (walk/postwalk #(if (keyword? %) (ns-qualify %) %) items)]
    (if (every? #(s/valid? ::item %) qitems)
      items
      (do (doall (map #(s/explain ::item %) qitems))
          (throw-error "Items are not valid")))))

(def bronze
  {:name-prefix "Bronze"
   :set-name :bronze
   :quality 1
   :type :plate
   :level 5
   :stats {:str 2 :vit 2 :sta 1 :spi 1 :mr 1}
   :armor [0.9 1.2]
   :num-stats [2 3]
   :instances [{:slot :arms :name "Vambraces"}
               {:slot :chest :name "Breastplate"}
               {:slot :feet :name "Boots"}]})

(def exe-axe
  {:name "Executioner's Axe"
   :quality 1.3
   :level 10
   :stats {:str 1 :vit 1 :sta 1}
   :num-stats [1 2]
   :delay 3
   :slot :main-hand
   :two-hand true
   :type :axe})

(def items
  (flatten
   (map
    expand-item-set
    [bronze
     exe-axe])))