(ns game.item-generation
  (:require
   [clojure.math.combinatorics :as comb]
   [game.constants :as consts]
   [game.item-names :as names]
   [game.math :as math]
   [game.stats-and-items :as si]
   [game.utils :refer :all]))

(def base-wants
  {:spirit 1
   :armor 1
   :vitality 1
   :magic-resistance 1})

(def caster-wants
   {:intelligence 1
    :wisdom 1})

(def wants-maps
  (fmap #(merge % base-wants)
        {:warrior {:strength 1 :stamina 1}
         :wizard caster-wants
         :druid caster-wants
         :necromancer caster-wants
         :paladin {:strength 1 :wisdom 1}
         :ranger {:agility 1 :wisdom 1}
         :rogue {:agility 1 :stamina 1}}))

(defn select-random-keys [m num]
  (select-keys m (take num (shuffle (keys m)))))

(defn normalize-map [m]
  (let [total (apply + (vals m))]
    (fmap #(/ % total) m)))

(defn armor? [type]
  (#{:cloth :leather :mail :plate} type))

(defn add-base-armor
  "Adds a base armor component in a normalized stats-dist map."
  [stats-dist]
  (-> (fmap #(* % (- 1 si/armor-ratio)) stats-dist)
    (update :armor +some si/armor-ratio)))

(defn create-stats-dist-with-armor
  "Given a stats-dist map (not necessarily normalized) and a type, create a
   normalized stats-dist map with a base armor component added if the type is an
   armor type."
  [stats-dist type]
   (cond-> (normalize-map stats-dist)
     (armor? type) add-base-armor))

(defn create-random-stats-dist
  "Create a random stats-dist map based on the class's wants, including base
   armor."
  [class type]
  (let [wants (wants-maps class)
        num-stats (rand-uniform-int 1 (count wants))]
    (create-stats-dist-with-armor (select-random-keys wants num-stats) type)))

(defn final-item-stats [slot type stats-dist level quality]
  (let [stats-factor (* (slot si/relative-gear-slot-value) quality)
        stats-amount (* si/stats-per-slot-per-level level stats-factor)]
    (->>$ stats-dist
      (fmap #(* stats-amount %))
      (update $ :armor *some (si/armor-factor type))
      (fmap math/round))))
