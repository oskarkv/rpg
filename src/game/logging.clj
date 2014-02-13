(ns game.logging
  (:require [robert.hooke :as rh]
            [clojure.pprint :as pp]
            [game.server.core :as sv]
            [game.client.core :as cl]
            [game.math :as math]
            (game.common [core-functions :as ccfns]
                         [graphics :as gfx])
            (game.server [pathfinding :as pf]))
  (:use [game.utils]))

(defn log-println [name type object]
  (println (str (current-thread-name) " // " name " " type ":\n"
                (with-out-str
                  (pp/pprint object)))))

(defn print-output [name f & args]
  (let [result (apply f args)]
    (log-println name "output" result)
    result))

(defn print-input [name f & args]
  (log-println name "input" args)
  (apply f args))

(defn print-call [name f & args]
  (println (str (current-thread-name) "//" name))
  (apply f args))

(defmacro make-name-var-list [fn-list]
  `[~@(for [fn fn-list]
        [(str fn) `(var ~fn)])])

(defmacro defloglist [name & fns]
  `(def ~name (make-name-var-list [~@fns])))

(defn add-hooks [name-vars & wrappers]
  (when (seq wrappers)
    (doseq [[name var] name-vars]
      (rh/add-hook var (partial (first wrappers) name)))
    (recur name-vars (next wrappers))))

(defn get-ns-name-vars [ns-sym]
  (-> (the-ns ns-sym) (#(.name %)) ns-interns))

(defn add-hooks-to-ns [ns-sym & wrappers]
  (apply add-hooks (get-ns-name-vars ns-sym) wrappers))

(defloglist log-both)

(defloglist log-input)

(defloglist log-output)

(defn add-logging-wrappers []
  (dorun (->> (all-ns) (map #(.name %)) (mapcat ns-interns) (map second)
              (map rh/clear-hooks)))
  (add-hooks log-both print-output print-input)
  (add-hooks log-input print-input)
  (add-hooks log-output print-output))
