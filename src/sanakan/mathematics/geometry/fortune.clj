(ns sanakan.mathematics.geometry.fortune
  (:require
    [clojure.zip :as z]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.parabola :as par]
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

(defrecord TreeNode [point parabola left right]
  c/Printable
  (c/out [this i] (str (c/indent i) "Node at " (c/out point) " has "
                       (if (nil? parabola) "no parabola yet " (c/out parabola))
                       "\n" (c/indent (+ i 2)) (if (nil? left) "-" (c/out left)) " <-> " (if (nil? right) "-" (c/out right)) "\n"))
  (c/out [this] (c/out this 0)))


;; define the data structure we need to represent a voronoi diagram.
(defrecord Voronoi [points events tree step]
  c/Printable
  (c/out [this i] (str (c/indent i) "Voronoi step " step " for the points\n"
                       (reduce str (interpose "\n" (for [p points] (c/out p (+ i 2)))))
                       "\n\nconsists of queued events\n"
                       (reduce str (interpose "\n" (for [e events] (c/out e (+ i 2)))))
                       (if (nil? tree)
                         "\n\nand no tree"
                         (str "\n\nand the tree\n" (c/out tree (+ i 2))))
                       "\n\n"))
  (c/out [this] (c/out this 0)))

(defn add-parabola
  [tree point]
  (if (nil? tree)
    (TreeNode. point nil nil nil)
    tree))

(defn remove-parabola
  [tree point]
  tree)

(defn step
  "Process first event in events and update event queue and tree."
  [v]
  (let [e (first (:events v))
        tree (:tree v)]
    (Voronoi.
      (:points v)
      (rest (:events v))
      (if (= :site (:t e))
        (add-parabola tree e)
        (remove-parabola tree e))
      (+ 1 (:step v)))))

(defn voronoi
  [points]
  (Voronoi. points (events points) nil 0))
