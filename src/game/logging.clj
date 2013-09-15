(ns game.logging
  (require [robert.hooke :as rh]
           [clojure.pprint :as pp]
           [game.server.core :as sv]
           [game.common.core-functions :as ccfns])
  (use [game.utils]))

(defn print-output [name f & args]
  (let [result (apply f args)]
    (println (str (current-thread-name) " // " name
                  " output:"))
    (pp/pprint result)
    result))

(defmacro make-name-var-list [fn-list]
  `[~@(for [fn fn-list]
        [(str fn) `(var ~fn)])])

(def log-output (make-name-var-list [sv/main-update
                                     ccfns/process-network-msgs]))

(defn add-logging-wrappers []
  (doseq [[name var] log-output]
    (rh/clear-hooks var)
    (rh/add-hook var (partial print-output name))))
