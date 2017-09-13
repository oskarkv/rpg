(ns game.item-generation
  (:require
   [game.utils :refer :all]))

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
         :druid caster-wants}))
