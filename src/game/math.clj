(ns game.math)

(defn normalize [v]
  (let [len (Math/sqrt (apply + (map #(* % %) v)))]
    (map (if (zero? len) identity #(/ % len)) v)))

(defn norm-diff [v v2]
  (normalize (map - v v2)))

(defn distance [p p2]
  (Math/sqrt (apply + (map #(* % %) (map - p p2)))))

(defn extrapolate-pos [pos dir time speed]
  (map + pos (map #(* speed time %) dir)))

(defn dot-product [v v2]
  (apply + (map * v v2)))
