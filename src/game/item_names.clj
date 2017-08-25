(ns game.item-names
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [game.hierarchies :as h]
   [game.utils :refer :all]))

(def object-names-map
  ;;; Held
  ;; Melee
  {:any nil
   #_(mapv
    #(str % "")
    ["Secret, s" "Madness" "Symbol, s" "Focus" "Insanity" "Might" "Power"
     "Touch" "Sacrifice" "Promise, s" "Burden" "Will" "Tear, s" "Honor"
     "Fear, s" "Darkness" "Anger" "Courage" "Scream, s" "Torture"
     "Revelation, s" "Malice" "Malevolance" "Justice" "Embrace" "Legacy"])
   :weapon ["Persuader" "Destroyer" "Mutilator" "Doombringer" "Slayer"
            "Ravager"]
   :mace ["Mace" "Maul" "Smasher" "Pummeler" "Bonecracker" "Skullcracker"
          "Beater" "Bludgeon" "Shatterer" "Demolisher" "Grinder" "Bonegrinder"]
   :1h-mace ["Morning Star" "Hammer" "Mallet" "Cudgel" "Gavel"]
   :2h-mace ["Bonecrusher" "Skullcrusher" "Two-handed Mace" "Sledge"
             "Sledgehammer"]
   :blade ["Blade" "Edge" "Kris" "Gutter" "Slicer" "Eviscerator" "Disemboweler"
           "Render"]
   :sword ["Sword" "Glaive"]
   :1h-sword ["Thrasher" "Bastard Sword" "Broadsword"]
   :2h-sword ["Greatsword" "Claymore" "Two-handed Sword" "Longsword"]
   :staff ["Staff" "Cane" "Quarterstaff"]
   :dagger ["Dagger" "Shiv" "Piercer" "Ripper" "Shanker" "Dirk" "Stiletto"
            "Poniard"]
   :spear ["Spear" "Pike" "Impaler"]
   :axe ["Axe" "Cleaver" "Decapitator" "Chopper"]
   :1h-axe ["Hacker" "Hatchet"]
   :2h-axe ["Broadaxe" "Greataxe" "Headsplitter" "Splitter" "Two-Handed Axe"]
   :caster-offhand
   ["Chalice" "Book" "Charm" "Crystal" "Globe" "Globule" "Goblet" "Grimoire"
    "Idol" "Lantern" "Ledger" "Omen" "Orb" "Relic" "Scepter" "Shard" "Skull"
    "Talisman" "Urn" "Lexicon" "Spellbook" "Sphere" "Eye" "Jewel" "Tome"
    "Gem" "Flask"]
   ;; Ranged
   :bow ["Bow" "Longbow"]
   :wand ["Wand"]
   :crossbow ["Crossbow" "Spitter" "Impaler" "Boltspitter" "Arbalest"]
   ;; Shield
   :shield ["Shield" "Barrier" "Aegis"]
   ;;; Worn
   ;; Jewelry
   :finger ["Ring" "Band" "Signet" "Seal"]
   :ear ["Earring" "Loop" "Pendant"]
   ;; Armor
   :back ["Cape" "Cloak" "Mantle"]
   :cloth ["Cloth"]
   :mail ["Mail" "Chain" "Ringmail" "Chainmail"]
   :plate ["Plate"]
   :leather ["Leather" "Hide"]
   :arms
   {:any []
    :cloth ["Sleeves"]
    :leather ["Armband" "Armguards"]
    :mail ["Sleeves" "Armguards"]
    :plate ["Armplates" "Vambraces" "Armguards"]}
   :wrist
   {:any ["Bracer" "Wristguard"]
    :jewelry ["Bracelet"]
    :cloth ["Armwrap" "Cuff" "Wristwrap" "Wristband"]
    :leather ["Wristband" "Wristwrap"]
    :mail ["Wristband"]
    :plate ["Vambrace" "Wristplate"]}
   :chest
   {:any ["Chestguard"]
    :cloth ["Shirt" "Robe" "Robes" "Vestment" "Raiment"]
    :leather ["Harness" "Tunic" "Vest" "Jerkin" "Cuirass" "Armor" "Coat"]
    :mail ["Hauberk" "Armor" "Chainmail" "Ringmail" "Cuirass"]
    :plate ["Chestplate" "Breastplate" "Chestpiece" "Armor" "Cuirass" "Aegis"]}
   :face
   {:any ["Faceguard" "Cover"]
    :cloth ["Veil" "Mask"]
    :leather ["Mask"]
    :mail ["Mask" "Facechain" "Veil"]
    :plate ["Visor" "Faceplate"]}
   :feet
   {:any []
    :cloth ["Sandals" "Slippers" "Footwraps" "Shoes" "Treads"]
    :leather ["Treads" "Footpads" "Moccasins" "Boots" "Slippers" "Striders"
              "Pathfinders"]
    :mail ["Sabatons" "Boots"]
    :plate ["Greaves" "Sabatons" "Stompers"]}
   :hands
   {:any []
    :cloth ["Gloves" "Handwraps"]
    :leather ["Gloves" "Handguards" "Grips" "Grasps"]
    :mail ["Gauntlets" "Handguards"]
    :plate ["Gauntlets" "Handguards"]}
   :head
   {:any []
    :cloth ["Hood" "Cowl" "Hat" "Headdress" "Skullcap"]
    :leather ["Hood" "Headdress" "Helm" "Coif" "Skullcap"]
    :mail ["Helm" "Coif" "Headgear"]
    :plate ["Helm" "Helmet" "Headgear" "Barbute" "Greathelm"]}
   :legs
   {:any ["Legguards" "Leggings"]
    :cloth ["Trousers" "Legwraps" "Pantaloons"]
    :leather ["Trousers" "Legwraps"]
    :mail ["Kilt"]
    :plate ["Greaves" "Cuisse"]}
   :neck
   {:any ["Neckguard" "Gorget"]
    :jewelry ["Chain" "Necklace" "Pendant" "Amulet" "Talisman" "Charm"
              "Medallion" "Medal"]
    :cloth ["Choker"]
    :leather []
    :mail ["Aventail" "Camail"]
    :plate ["Bevor"]}
   :shoulders
   {:any []
    :cloth ["Shoulderpads" "Mantle" "Amice" "Epaulets"]
    :leather ["Shoulderpads" "Spaulders" "Pauldrons" "Shoulderguards" "Mantle"]
    :mail ["Spaulders" "Pauldrons" "Shoulderguards" "Mantle"]
    :plate ["Spaulders" "Pauldrons" "Shoulderguards"]}
   :waist
   {:any ["Girdle"]
    :cloth ["Cord" "Cinch" "Sash" "Belt" "Waistband"]
    :leather ["Cord" "Cinch" "Belt" "Waistband"]
    :mail ["Waistguard" "Clasp"]
    :plate ["Waistguard" "Clasp"]}})

(def things-and-adjective
  {:any
   {:adjective ["Great" "Strong"]}
   :living
   {:adjective ["Swift"]
    :things ["Cat"]}})


(def cruel
  ["Vicious"
   "Cruel"
   "Wicked"])

(def brave
  ["Brave"
   "Courageous"])

(def aggressive
  ["Ruthless"
   "Ferocious"
   "Vengeful"
   "Merciless"
   "Bloodthirsty"
   "Rampaging"
   "Brutal"
   "Furious"])

(def enlightened
  ["Awakened"
   "Enlightened"
   "Insightful"])

(def wise
  ["Wise"
   "Sage"
   "Erudite"
   "Scholarly"])

(def clever
  ["Clever"
   "Shrewd"
   "Cunning"])

(def feared
  ["Feared"
   "Dreaded"
   "Terrifying"
   "Frightful"])

(def weak
  ["Feeble"
   "Weak"
   "Fragile"])

(def owners
  [[["Wise" "Just" "Brave" "Courageous"] ["Knight" "Warrior"]]
   [["Cunning" "True" "Old" cruel] ["Wizard" "Sorcerer" "Sorceress"]]
   [[aggressive cruel clever] ["Brawler" "Fighter" "Gladiator" "Warrior"]]
   [[feared] ["Gladiator" "Warrior" "Guardian"]]])

(def virtues
  ["Justice"
   "Preseverance"
   "Wisdom"
   "Patience"
   "Courage"
   "Bravery"
   "Strength"
   "Temperance"
   "Mercy"
   "Kindness"
   "Discipline"
   "Determination"
   "Tenacity"
   "Truthfulness"
   "Resolve"
   "Foresight"])

(def other-abstract
  ["Transcendence"
   "Initiative"
   "Dreams"
   "Alacrity"
   "Death"
   "Life"
   "Glory"
   "Conquest"
   "Prophecy"
   "Hatred"
   "Harmony"
   "Peace"
   "Tranquility"
   "Unity"
   "Plunder"
   "Insight"
   "Intellect"])

(def abstract-suffixes
  (into
   virtues
   other-abstract))

(def adjective
  ["Crippling"
   "Stinking"
   "Ceremonial"
   "Flickering"
   "Glowing"
   "Rising"
   "Breaking"
   "Empowered"
   "Blinding"
   "Blazing"
   "Infused"
   "Enchanced"
   "Impending"
   "Imminent"
   "Fulminating"
   "Exploding"
   "Deadly"
   "Imploding"
   "Pure"
   "Grim"
   "Cursed"
   "Damned"
   "Blessed"
   "Rotting"
   "Devouring"
   "Ruinous"
   "Fallen"
   "Unstable"])

(def other-nouns
  ["Beacon"
   "Assault"
   "Wave"
   "Fury"
   "Light"
   "Darkness"
   "Hell"
   "Heaven"
   "Haven"
   "Serenity"
   "Faith"
   "Doubt"
   "Anguish"
   "Plague"
   "Golem"
   "Grave"
   "Calamity"
   "Agony"
   "Ascension"
   "Pandemonium"])

(def races
  ["Troll"
   "Goblin"
   "Kobold"
   "Skeleton"
   "Elf"
   "Dwarf"
   "Dragon"
   "Drow"
   "Halfling"
   "Gnome"
   "Giant"
   "Orc"
   "Fairy"
   "Barbarian"
   "Undead"
   "Ghoul"])

;;; Some random words
"Sage"
"Scholar"
"Prodigy"
"Vigor"
"Fairy"
"Unyielding"
"Unwavering"
"Beyond"
"Sweeping"
"Forbidden"
"Timeless"
"Falling"
"Rising"
"Radiance"
"Flesh"
"Burn"
"Divine"
"Essence"
"Contamination"
"Final"
"Embrace"
"Ruthless"
"Reckless"
"Command"
"Frenzy"
"Enforcer"
"Decay"
"Rot"
"Blight"
"Astral"
"Infimidating"
"Terrifying"
"Marauder"
"Invigorating"
"Thriving"
"Berserker"
"Bloodthirst"
"Tough"
"Brawler"
"Juggernaut"
"Pestilence"
"Growing"
"Fortitude"
"Gruesome"
"Rampage"
"Vile"
"Frightening"
"Barren"

(def power-like
  ["Power"
   "Potency"])

(def insight
  ["Clarity"
   "Insight"])

(def sins
  ["Sloth"
   "Pride"
   "Envy"
   "Greed"
   "Wrath"])

(def nature-names
  ["t Sun"
   "t Moon"
   "Nature s"
   "Storm"
   "Rain"
   "Blizzard"
   "Hailstorm"
   "Thunder"
   "Lightning"])

(def animal-names
    ;;; Birds
  ["t Falcon"
   "t Eagle"
   "t Raven"
   ;;; Mammals
   "t Bear"
   "t Wolf"
   "t Cat"
   "t Tiger"
   ;;; Other
   "Scorpion"
   "t Snake"
   "t Frog"
   "t Lizard"])

(def plant-names
  ["Oak"
   "Tree"
   "t Sapling"
   "t Grove"
   "t Meadow"
   "t Thicket"
   "t Wood"
   "t Woodland"])

(def colors
  ["Silver"
   "Golden"
   "Black"
   "Red"])

(def gems
  ["Amber"
   "Sapphire"
   "Amethyst"
   "Pearl"
   "Turquoise"
   "Emerald"
   "Lapis Lazuli"
   "Bloodstone"
   "Jade"
   "Diamond"
   "Opal"
   "Onyx"
   "Moonstone"
   "Topaz"
   "Ruby"
   "Agate"
   "Peridot"
   "Malachite"
   "Pyrite"
   "Tiger's Eye"
   "Jasper"
   "Aquamarine"
   "Zircon"
   "Rose Quartz"
   "Citrine"])

(def fine-metal
  ["Gold"
   "Silver"
   "Platinum"])

(def strong-metal
  ["Steel"
   "Iron"
   "Mithril"
   "Adamantite"
   "Thorium"
   "Titanium"
   "Bronze"
   "Dragon Bone"
   "Dragon Scale"
   "Wyrmscale"])

(def adjective
  ["Ancient"
   "Arcane"
   "Blessed"
   "Bloody"
   "Dark"
   "Cruel"
   "Barbed"
   "Righteous"
   "Fearless"
   "Ferocious"
   "Feral"
   "Fabled"
   "Fine"
   "Heavy"
   "Light"
   "Mighty"
   "Mystic"
   "Mythical"
   "Old"
   "Ominous"
   "Potent"
   "Powerful"
   "Primordial"
   "Sacred"
   "Scholarly"
   "Sinister"
   "Ultimate"
   "Vigilant"])

{:owner []
 :adjective []
 :material []
 :material-2 []
 :kind []
 :type []}

(def affixes
  {:any
   {:owner ["Guardian" "Protector" "Goblin King" "Chieftan" "Defender"
            "Legion" "Adventurer"]
    :adjective ["Ornate" "Decorated" "Fine" "Crafted"]
    :material []
    :material-2 []
    :kind []
    :type []}
   ;;; Class-based
   :caster-class nil
   :int-caster-class
   {:owner ["Sage" "Wizard" "Sorcerer" "Erudite" "Prodigy" "Invoker"]
    :suffixes ["Power" "Potency"]
    :adjective ["Potent" "Magic" "Mystic"]}
   :healer nil
   :agile nil
   :tank nil
   ;;; Specific classes
   :warrior
   {:owner ["t Warrior s" "t Myrmidon s" "t Champion s"]
    :type ["Battle" "War"]
    :adjective ["War-torn"]}
   :druid
   {:owner ["Nature s" "t Naturewalker s" "t Druid s" "t Elder s"]}
   :wizard nil
   ;; ...

   ;;; Armor types
   :cloth
   {:material ["Cloth" "Silk" "Satin"]}
   :leather
   {:material ["Leather" "Hide" "Hardened Leather" "Rugged Leather"]}
   :metal
   {:material strong-metal
    :adjective ["Strong" "Unbreakable" "Sturdy"]}
   :mail
   {:material strong-metal
    :kind ["Chain" "Chainmail" "Ringmail" "Mail"]}
   :plate
   {:kind ["Plate"]
    :adjective ["Impenertable"]}
   :jewelry
   {:adjective ["Shiny" "Exquisite" "Precious"]
    :material ["Amber" "Sapphire" "Amethyst" "Pearl" "Turquoise" "Emerald"
               "Lapis Lazuli" "Bloodstone" "Jade" "Diamond" "Opal" "Onyx"
               "Moonstone" "Topaz" "Ruby" "Agate" "Peridot" "Malachite"
               "Pyrite" "Tiger's Eye" "Jasper" "Aquamarine" "Zircon"
               "Rose Quartz" "Citrine"]
    :material-2 fine-metal}
   ;;; Weapon Types
   :blunt
   {:adjective ["Spiked"]}

   :sharp
   {:adjective ["Sharp" "Gleaming" "Razor-Sharp" "Keen" "Sharpened" "Whetted"
                "Serrated" "Jagged" "Blunt"]}
   :ranged
   {:owner ["Marksman"]
    :adjective ["True"]}
   :melee nil
   :off-hand nil

   ;; Specific types
   :sword nil
   :staff
   {:adjective ["Gnarly" "Straight"]}
   :dagger
   {:adjective ["Light-Weight"]}
   :spear nil
   :axe nil
   :mace nil
   :caster-offhand nil
   ;; Ranged
   :bow nil
   :wand nil
   :crossbow nil
   ;; Shield
   :shield nil})

(defn prepend-of [thing]
  (str "of " thing))

(defn get-object-names [slot type]
  (let [slot-value (object-names-map slot)]
    (-> []
      (into (object-names-map type))
      (into (object-names-map :any))
      (into (if (map? slot-value)
              (concat (slot-value type) (slot-value :any))
              slot-value)))))

(def grammar
  (let [kind-part (mapv #(list `maybe %) [:material-2 :material :kind])]
  `{:base
    ([(maybe [:owner "'s"])
      (maybe :adjective)
      (~kind-part (maybe :type))
      :name
      (maybe :non-owner-suffix)]
     [(maybe :adjective)
      (~kind-part (maybe :type))
      :name
      (maybe :suffix)])
    :suffix (:owner-suffix :non-owner-suffix)
    :owner-suffix ["of the" :owner]
    :non-owner-suffix ["of" :noun]}))

(defn safe-rand-nth [s]
  (if (empty? s) nil (rand-nth s)))

(defn generate-grammar-part [parts-map part]
  (let [gen #(generate-grammar-part parts-map %)]
    (cond
      (and (list? part) (= (first part) `maybe)) (if (chance 0.6)
                                                   (gen (second part)))
      (vector? part) (map gen part)
      (sequential? part) (gen (safe-rand-nth part))
      (grammar part) (gen (grammar part))
      (string? part) part
      :else (safe-rand-nth (part parts-map)))))

(defn generate-name-parts
  "Generate a sequnece of name parts with at least n parts."
  [parts-map n]
  (let [g (generate-grammar-part parts-map :base)
        parts (count (remove #(or (nil? %) (every? nil? %)) g))]
    (if (>= parts n)
      g
      (recur parts-map n))))

(defn make-name [type slot class level rarity]
  (let [object-names (get-object-names slot type)
        tags (conj (h/type-tags type) type :any)
        parts-map (assoc (apply merge-with into (map affixes tags))
                         :name object-names
                         :noun abstract-suffixes)]
    (->>$ (generate-name-parts parts-map 2)
      flatten
      (remove nil?)
      (interpose " ")
      (apply str)
      (str/replace $ #" 's" "'s")
      str/trim)))
