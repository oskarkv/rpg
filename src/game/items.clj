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

(defn all-info
  "Merges the full info of an item into a light version of it."
  [light-item]
  (merge (items (:id light-item)) light-item))

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

(def items [])
