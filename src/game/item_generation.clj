(ns game.item-generation
  (:require
   [clojure.math.combinatorics :as comb]
   [game.constants :as consts]
   [game.item-names :as names]
   [game.math :as math]
   [game.stats :as stats]
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
  (-> (fmap #(* % (- 1 stats/armor-ratio)) stats-dist)
    (update :armor +some stats/armor-ratio)))

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
        num-stats (rand-uniform-int 2 (count wants))]
    (create-stats-dist-with-armor (select-random-keys wants num-stats) type)))

(defn final-item-stats
  "Creates the final stats of an item by taking into consideration all the
   arguments. stats-dist should be the final, normalized, stats-dist map after
   armor has been added."
  [slot type stats-dist level quality]
  (let [stats-factor (* (slot stats/relative-gear-slot-value) quality)
        stats-amount (* stats/stats-per-slot-per-level level stats-factor)]
    (->>$ stats-dist
      (fmap #(* stats-amount %))
      (update $ :armor *some (stats/armor-factor type))
      (fmap math/round))))

(defn make-item [name slot type stats-dist level quality]
  (let [stats-dist* (create-stats-dist-with-armor stats-dist type)]
    (if (> (:armor stats-dist*) (* 3 stats/armor-ratio))
      (throw-ex "Too high armor on item " name)
      (assoc
       (final-item-stats slot type stats-dist* level quality)
       :quality quality
       :level level
       :item-level (math/round (* quality level))
       :slot slot
       :type type
       :name name))))
