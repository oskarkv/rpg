(ns game.math)

(defn normalize [v]
  (let [len (Math/sqrt (apply + (map #(* % %) v)))]
    (map (if (zero? len) identity #(/ % len)) v)))

(defn distance [p p2]
  (Math/sqrt (apply + (map #(* % %) (map - p p2)))))
