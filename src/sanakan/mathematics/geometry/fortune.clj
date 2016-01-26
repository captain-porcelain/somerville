(ns sanakan.mathematics.geometry.fortune
  (:require
    [clojure.zip :as z]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.point :as p]))

;; define a site event type.
(defrecord SiteEvent [p t]
  c/Printable
  (c/out [this i] (str (c/indent i) "Event " t " at " (c/out p)))
  (c/out [this] (c/out this 0)))

(defn event
  "Create a new event for a point p with type t."
  [p t]
  (SiteEvent. p t))

(defn sort-events
  "Sort events by y position."
  [events]
  (sort-by #(:y (:p %)) events))

(defn events
  "Create sorted events for a list of points."
  [points]
  (sort-events (map #(event % :site) points)))

;; define the data structure we need to represent a voronoi diagram.
(defrecord Voronoi [events tree])

(defn add-parabola
  [tree point]
  tree)

(defn remove-parabola
  [tree point]
  tree)

(defn step
  "Process first event in events and update event queue and tree."
  [v]
  (let [e (first (:events v))
        tree (:tree v)]
    (Voronoi.
      (rest (:events v))
      (if (= :site (:t e))
        (add-parabola tree (:p e))
        (remove-parabola tree (:p e))))))

(defn voronoi
  [points]
  (Voronoi. (events points) []))
