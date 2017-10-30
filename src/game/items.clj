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

(defn expand-slot
  "Creates a sequence of left and right version of the slot if possible,
   else just wraps the slot in a vector."
  [slot]
  (if (hier/left-right-slots slot)
    (hier/left-right slot)
    [slot]))

(defn correct-slot? [item path]
  (if (nil? item)
    true
    (let [slot (peek path)]
      (if (contains? hier/actual-gear-slots slot)
        (contains? (set (expand-slot (:slot item))) slot)
        true))))

(defn get-tooltip [item]
  (let [{:keys [type damage delay stats slot quality]} item]
    (->>$ [(:name item)
           (when slot (str "Slot: " (name slot)))
           (when type (str "Type: " (name type)))
           (str "Quality: " (format "%.3f" quality))
           (when damage (str "Damage / Delay: " damage " / " delay))]
      (into $ (for [[s v] stats]
                (str (name s) ": " v)))
      (remove nil?)
      (str/join "\n"))))

(def items [])
