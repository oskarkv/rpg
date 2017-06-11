(ns game.item-generation)

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

(def adjective
  ["Dark"
   "Bloody"
   "Black"
   "Red"
   "Silver"
   "Sinister"
   "Heavy"
   "Light"
   "Ominous"
   "Old"])


(def nature-names
  ["Sun"
   "Moon"
   "Nature"])

(defn prepend-of [thing]
  (str "of " thing))

(def names
  :classes
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
             []})

(defn make-name [type slot class level rarity]
  (
