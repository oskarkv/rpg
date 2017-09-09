(ns game.item-generation
  (:require
   [clojure.math.combinatorics :as comb]
   [game.constants :as consts]
   [game.item-names :as names]
   [game.math :as math]
   [game.stats :as stats]
   [game.utils :refer :all]
   [clojure.string :as str]))

(def base-wants
  {:spi 1
   :armor 1
   :vit 1
   :mr 1})

(def caster-wants
   {:int 1
    :wis 1})

(def wants-maps
  (fmap #(merge % base-wants)
        {:warrior {:str 1 :sta 1}
         :wizard caster-wants
         :druid caster-wants
         :necromancer caster-wants
         :paladin {:str 1 :wis 1}
         :ranger {:agi 1 :wis 1}
         :rogue {:agi 1 :sta 1}}))

(defn normalize-map [m]
  (let [total (apply + (vals m))]
    (fmap #(/ % total) m)))

(defn scale-map [m scale]
  (fmap #(* scale %) m))

(defn armor? [type]
  (#{:cloth :leather :mail :plate} type))

(defn armor-factor-to-part
  "Takes a stats distribution map with a special armor factor. Normalizes the
   stats distribution map including an armor part based on the armor factor."
  [stats]
  (let [armor-factor (or (:armor stats) 0)
        armor-part (* stats/armor-ratio armor-factor)]
    (-> (dissoc stats :armor)
      normalize-map
      (scale-map (- 1 armor-part))
      (assoc :armor armor-part))))

(defn final-item-stats
  "Creates the final stats of an item by taking into consideration all the
   arguments. stats should be a distribution map with an armor factor."
  [slot type stats level quality]
  (let [stats* (armor-factor-to-part stats)
        stats-factor (* quality level
                        stats/stats-per-slot-per-level
                        (stats/relative-gear-slot-value slot))]
    (->>$ stats*
      (fmap #(* stats-factor %))
      (update $ :armor *some (stats/armor-factor type))
      (fmap math/round))))

(defn make-item
  "Make an item by taking an item-map, which is almost an item, and calculates
   the actual stats, adds damage for weapons, and adjusts quality using
   extra-rarity."
  [item-map]
  (let [{:keys [slot type stats level quality extra-rarity delay two-hand
                name set-name]} item-map
        stats-quality (* (if two-hand stats/relative-two-hander-value 1)
                         quality)]
    (cond-> item-map
      true (assoc :quality (+some quality extra-rarity)
                  :stats (final-item-stats slot type stats level stats-quality))
      true (dissoc :extra-rarity)
      delay (assoc :damage (* stats-quality delay (stats/weapon-dps level))))))

(defn make-name [prefix name suffix]
  (str/trim (str prefix " " name " " suffix)))

(defn handle-name [item-map]
  (let [{:keys [name-prefix name name-suffix]} item-map]
    (-> item-map
      (assoc :name (make-name name-prefix name name-suffix))
      (dissoc :name-prefix :name-suffix))))

(defn make-item-set [set-name defaults item-maps]
  (let [{:keys [name-prefix name-suffix]} defaults]
    (map
     (fn [item-map]
       (-> (merge defaults item-map)
         (assoc :set-name set-name)
         handle-name
         make-item)
       item-maps))))

(def items
  (flatten
   [(make-item-set
     :bronze
     {:name-prefix "Bronze"
      :quality 1.2
      :type :plate
      :level 5
      :stats {:str 1 :vit 1 :mr 0.5 :armor 1.3}}
     [{:slot :arms :name "Vambraces"}
      {:slot :chest :name "Breastplate"}
      {:slot :feet :name "Boots"}])]))
