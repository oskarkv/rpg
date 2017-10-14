(ns game.client.base
  (:require
   [game.common.core-functions :as ccfns]))

(def event-queue
  "Emptied and processed after each step in the game loop."
  (ref []))

(def all-events-queue
  "Collects all events in an interation of the game loop. The events are used
   for calling update on the subsystems."
  (ref []))

(def enqueue-events (ccfns/enqueue-fn event-queue all-events-queue))

(defmulti process-event (fn [game-state event] (:type event)))

(defmethod process-event :default [game-state event])
