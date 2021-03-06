(ns game.item-generation
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [game.constants :as consts]
   [game.hierarchies :as hier]
   [game.math :as math]
   [game.stats :as stats]
   [game.utils :refer :all]))

(def base-wants
  {:spi 1
   :vit 1
   :mr 1})

(def caster-wants
  {:int 1
   :wis 1})

(def wants-maps
  (fmap #(merge % base-wants)
        {:warrior {:str 1 :sta 1}
         :wizard caster-wants
         :druid caster-wants}))

(def classes (vec (keys wants-maps)))

(def slot-chances (merge (zipmap hier/body-slots (repeat 1))
                         (zipmap hier/held-slots (repeat 3))))

(defn random-type [class slot]
  (let [type (-> (set/intersection
                  (hier/slot->types slot)
                  (set/union (hier/class->held-types class)
                             #{:cloak :jewelry :armor}))
               vec rand-nth)]
    (if (= type :armor)
      (hier/class->armor-type class)
      type)))

(defn random-class-slot-type []
  (let [class (rand-nth classes)
        slot (random-pick slot-chances)
        type (random-type class slot)]
    (make-map class slot type)))

(defn create-abstract-item [level quality]
  (let [{:keys [class slot type]} (random-class-slot-type)
        wants (wants-maps class)
        item {:quality quality
              :level level
              :stats wants
              :type type
              :slot slot
              :armor [0.7 1.3]
              :num-stats [1 (min 5 (count wants))]}]
    (cond-> item
      (hier/weapon? type) (assoc :delay 3))))

(defn normalize-map [m]
  (let [total (apply + (vals m))]
    (fmap #(/ % total) m)))

(defn scale-map [m scale]
  (fmap #(* scale %) m))

(defn armor-factor-to-part
  "Takes a stats distribution map and an armor factor, and adds an armor part
   to the stats map based on the armor factor. The armor factor is how much
   of the standard amount of armor the item should have."
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
  "Spread the stats in stats-map out over num-items items so that the sum of
   the stats for the items have the same distribution as stats-map."
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

(defn randomly-adjust-stats
  "Takes 25% of every stat and places it in a pool, then randomly redistributes
   the pool among the stats."
  [stats]
  (let [to-take 0.25
        taken (* to-take (apply + (vals stats)))
        n (count stats)]
    (->>$ (repeatedly (dec n) rand)
      (cons 0) (sort >) (map - (cons 1 $) $) (map #(* taken %))
      (fmap + (fmap #(* (- 1 to-take) %) stats)))))

(defn remove-zero-stats [stats]
  (select-keys stats (for [[k v] stats :when (not (zero? v))] k)))

(defn real-item-stats
  "Creates a map of stats for an item by taking into consideration all the
   relevant information from item-map. The given quality is used, and not the
   quality in the item-map."
  [item-map quality]
  (let [{:keys [armor stats num-stats level slot type]} item-map
        n (value-from-range num-stats)
        stats-factor (* quality level
                        stats/stats-per-slot-per-level
                        (stats/relative-gear-slot-value slot))]
    (->$ stats
      (pick-stats-randomly n)
      (zipmap (repeat 1))
      randomly-adjust-stats
      (armor-factor-to-part (value-from-range armor))
      (fmap #(* stats-factor %) $)
      (update :armor *some (stats/armor-factor type))
      (fmap math/round $)
      remove-zero-stats)))

(defn random-numbers-with-mean
  "Returns n random numbers between min-v and max-v, with the given mean.
   If min-v and max-v are not provided, default to 0 and 1."
  ([mean n] (random-numbers-with-mean mean n 0 1))
  ([mean n min-v max-v]
   (when (pos? n)
     (let [temp-max (- max-v min-v)
           temp-mean (- mean min-v)
           vars (repeatedly n #(rand temp-max))
           vars-mean (/ (apply + vars) n)
           adjusting-fn (fn [mean target]
                          (if (> mean target)
                            #(* (/ target mean) %)
                            #(+ % (* (- temp-max %)
                                     (/ (- target mean) (- temp-max mean))))))]
       (map (comp #(+ min-v %) (adjusting-fn vars-mean temp-mean)) vars)))))

(defn create-real-item
  "Make an item by taking an item-map, which is almost an item, and calculates
   the actual stats, adds damage for weapons, and adjusts quality using
   extra-rarity."
  [item-map]
  (let [{:keys [quality extra-rarity delay two-hand level]} item-map
        stats-quality (* (if two-hand stats/relative-two-hander-value 1)
                         quality)]
    (cond-> item-map
      true (assoc :quality (+some quality extra-rarity)
                  :stats (real-item-stats item-map stats-quality))
      true (dissoc :extra-rarity :armor :num-stats)
      delay (assoc :damage (math/round (* stats-quality delay
                                          (stats/weapon-dps level)))))))

(def chance-denom
  "How many times more unlikely it is per 1 quality to get a drop."
  100)

(def base-drop-chance
  "What is the chance to get a quality 1 item."
  0.1)

(def max-quality 3.5)

(defn drop-chance
  "Returns the chance for an item of the given quality to drop from a normal
   mob. This is the inverse of drop-quality."
  [quality]
  (/ base-drop-chance (math/expt chance-denom (dec quality))))

(defn drop-quality
  "Returns the item quality that the given roll from 0 to 1 represents. A lower
   roll means better quality. This is the inverse of drop-chance."
  [roll-result]
  (if (zero? roll-result)
    max-quality
    (min max-quality (inc (/ (math/lg (/ base-drop-chance roll-result))
                             (math/lg chance-denom))))))

(defn generate-item [level]
  (let [quality (drop-quality (rand))]
    (when (>= quality 1)
      (-> level
        (create-abstract-item quality)
        create-real-item
        (assoc :name "Random item")))))
