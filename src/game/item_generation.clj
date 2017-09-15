(ns game.item-generation
  (:require
   [game.utils :refer :all]
   [game.hierarchies :as hier]
   [clojure.set :as set]))

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

(defn create-item [level quality]
  (let [{:keys [class slot type]} (random-class-slot-type)
        wants (wants-maps class)
        item {:quality quality
              :level level
              :stats wants
              :type type
              :slot slot
              :armor [0.8 1.2]
              :num-stats [1 (min 5 (count wants))]}]
    (cond-> item
      (hier/weapon? type) (assoc :delay 3))))
