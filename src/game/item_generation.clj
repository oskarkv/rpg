(ns game.item-generation
  (:require
   [game.constants :as consts]
   [game.utils :refer :all]))

(def animal-names
  ["Falcon"
   "Bear"
   "Wolf"
   "Eagle"
   "Snake"
   "Frog"
   "Cat"
   "Tiger"])

(def plant-names
  ["Oak"
   "Tree"
   "Sapling"
   "Grove"
   "Meadow"
   "Thicket"
   "Wood"
   "Woodland"])

(def colors
  ["Silver"
   "Black"
   "Red"])

(def metal
  ["Silver"
   "Steel"
   "Iron"
   "Mithril"])

(def adjective
  ["Dark"
   "Bloody"
   "Sinister"
   "Heavy"
   "Light"
   "Ominous"
   "Old"
   "Ultimate"
   "Powerful"
   "Fabled"
   "Mystic"
   "Mythical"
   "Arcane"
   "Sacred"
   "Vigilant"
   "Potent"
   "Mighty"
   "Blessed"
   "Primordial"
   "Ancient"
   "Old"
   "Fine"])

"of the
Sage
Scholar
Prodigy
Vigor
Fairy
"

"
Relic
Talisman
Idol
Shard
Omen
"

(def nature-names
  ["Sun"
   "Moon"
   "Nature"])

(defn prepend-of [thing]
  (str "of " thing))

;; Den här borde bero på nån hierarchy
(def names
  {:classes
   {:druid {:prefix
            ["Nature's"
             "Naturewalker's"
             "Druid's"
             "Elder's"]
            :suffix
            (concat
             (for [thing (concat plant-names animal-names)]
               (str "the " thing))
             nature-names)}
    :warrior {:prefix
              ["Warrior's"
               "Battle"
               "War"
               "Myrmidon's"
               "Champion's"]
              :suffix
              ["Battle"
               "Slaughter"
               "Victory"]}}
   :plate
   {:chest {:prefix
            []}}})

(defn make-name [type slot class level rarity]
  )

(def wanted-stats
  {:warrior {:strength 1
             :stamina 1
             :spirit 0.5
             :magic-resistance 1
             :armor 1}
   :druid {:intelligence 1
           :spirit 1
           :wisdom 0.5
           :magic-resistance 1
           :armor 0.5}})

;; pieces kan vara slots (ett set) eller typ :armor, :jewelry, asså nått som
;; finns i hierarchyn
(defn make-set
  [stats pieces armor-type]
  )

(defn create-item [slot stats level rarity]
  )

(defn create-items-for-class [amount class]
  )
