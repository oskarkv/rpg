(ns game.common.input
  (:use game.utils)
  (:import (com.jme3.input
             KeyInput
             MouseInput)
           (com.jme3.input.controls
             KeyTrigger
             ActionListener)))

(defn string->KeyInput [s]
  (-> (.getField KeyInput (str "KEY_" (.toUpperCase s))) (.get nil)))

(defn create-KeyInputs [key-bindings]
  (map (fn [[name key type]] [name (string->KeyInput key) type])
       key-bindings))

(defn create-key-state-map [key-bindings]
  (-> (zipmap (map (comp keyword first)
                   (filter (fn [[_ _ type]] (= type :hold)) key-bindings))
              (repeat false))
      (assoc :taps [])))

(defn empty-taps [key-state-atom]
  (swap! key-state-atom assoc :taps []))

(defn start-input [input-manager key-bindings key-state-atom]
  (let [keyinput-bindings (create-KeyInputs key-bindings)
        hold-listener (reify ActionListener
                        (onAction [this name pressed tpf]
                          (swap! key-state-atom assoc (keyword name) pressed)))
        tap-listener (reify ActionListener
                       (onAction [this name pressed tpf]
                         (when pressed
                           (swap! key-state-atom
                                  update-in [:taps] conj (keyword name)))))
        filter-fn (fn [type]
                    (into-array
                      (map first (filter #(= type (% 2)) key-bindings))))
        tap-names (filter-fn :tap)
        hold-names (filter-fn :hold)]
    (doseq [[name key _] keyinput-bindings]
      (.addMapping input-manager name (into-array [(KeyTrigger. key)])))
    (.addListener input-manager hold-listener hold-names)
    (.addListener input-manager tap-listener tap-names)))

