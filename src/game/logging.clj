(ns game.logging
  (require [robert.hooke :as rh]
           [clojure.pprint :as pp]
           [game.server.core :as sv]
           [game.client.core :as cl]
           (game.common [core-functions :as ccfns]
                        [graphics :as gfx]))
  (use [game.utils]))

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

(defmacro make-name-var-list [fn-list]
  `[~@(for [fn fn-list]
        [(str fn) `(var ~fn)])])

(def log-output (make-name-var-list []))

(def log-input (make-name-var-list []))

(defn add-logging-wrappers []
  (dorun (->> (all-ns) (map #(.name %)) (mapcat ns-interns) (map second)
              (map rh/clear-hooks)))
  (doseq [[name var] log-input]
    (rh/add-hook var (partial print-input name)))
  (doseq [[name var] log-output]
    (rh/add-hook var (partial print-output name))))
