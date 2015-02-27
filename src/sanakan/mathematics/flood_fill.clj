;; Provides a simple algorithm to flood fill an area
(ns sanakan.mathematics.flood-fill
  (:require
    [sanakan.mathematics.geometry.point :as p]))

(defn neighbour?
  "Are two points neighbours?"
  [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))
        dx (if (< dx 0) (* -1 dx) dx)
        dy (if (< dy 0) (* -1 dy) dy)]
    (or
      (and (= 1 dx) (= 1 dy))
      (and (= 1 dx) (= 0 dy))
      (and (= 0 dx) (= 1 dy)))))

(defn neighbours
  "Filter list of points to those that are neighbours of the given point."
  [points point]
  (filter #(neighbour? point %) points))

(defn neighbours2
  "Filter list of points to those that are neighbours of the given point."
  [point]
  (list
    (p/point (- (:x point) 1)  (- (:y point) 1))
    (p/point (- (:x point) 1)  (:y point))
    (p/point (- (:x point) 1)  (+ (:y point) 1))
    (p/point (:x point)  (- (:y point) 1))
    (p/point (:x point)  (+ (:y point) 1))
    (p/point (+ (:x point) 1)  (- (:y point) 1))
    (p/point (+ (:x point) 1)  (:y point))
    (p/point (+ (:x point) 1)  (+ (:y point) 1))))

(defn test-neighbours
  "Test all neighbours and return those that should be added."
  [point points decider-fn]
  (let [n (neighbours points point)]
    (filter #(decider-fn point %) n)))

(defn in-bounds?
  "Check if a pixel is inside an image."
  [p x1 y1 x2 y2]
  (and
    (<= (:x p) x2)
    (>= (:x p) x1)
    (<= (:y p) y2)
    (>= (:y p) y1)))

(defn test-neighbours2
  "Test all neighbours and return those that should be added."
  [point decider-fn x1 y1 x2 y2]
  (let [n (filter #(in-bounds? % x1 y1 x2 y2) (neighbours2 point))]
    (filter #(decider-fn point %) n)))

(defn fill
  "Start with the first point in the list of points and take as long as
  the decider function returns true for the results of the value function
  of the current point and the next one."
  [seed points decider-fn]
  (loop [newpoints (list seed) ;; put seed into candidates list
         i 0
         accepted '()]
    (if (= 0 (count newpoints))
      accepted
      (let [p (first newpoints) ;; take seed
            n (test-neighbours p points decider-fn) ;; and get fitting neighbours
            np (concat (rest newpoints) (remove (set (concat newpoints accepted)) n)) ;; filter neighbours to new ones
            ap (conj accepted p)
            ;tmp (dorun (println (str "run " i ": " (count np) " - " (count ap))))
            ]
        (recur np (+ i 1) ap)))))

(defn fill2
  "Start with the first point in the list of points and take as long as
  the decider function returns true for the results of the value function
  of the current point and the next one."
  [seed points decider-fn x1 y1 x2 y2]
  (loop [newpoints (list seed) ;; put seed into candidates list
         i 0
         accepted '()]
    (if (= 0 (count newpoints))
      accepted
      (let [p (first newpoints) ;; take seed
            n (test-neighbours2 p decider-fn x1 y1 x2 y2) ;; and get fitting neighbours
            np (concat (rest newpoints) (remove (set (concat newpoints accepted)) n)) ;; filter neighbours to new ones
            ap (conj accepted p)
            ;tmp (dorun (println (str "run " i ": " (count np) " - " (count ap))))
            ]
        (recur np (+ i 1) ap)))))

(defn partition
  "Use flood fill to partition a space and return a list of partitions with their points.
  The decider-fn should take a point and a neighbour and return true/false accordingly if
  the neighbour is reachable."
  [points decider-fn]
  (loop [remaining points
         i 0
         partitions '()]
    (if (= 0 (count remaining))
      partitions
      (let [seed (first remaining)
            testpoints (rest remaining)
            part (fill seed testpoints decider-fn)
            restpoints (remove (set part) testpoints)
            ;tmp (dorun (println (str "run " i ": " (count part) " - " (count restpoints))))
            ]
        (recur restpoints (+ i 1) (conj partitions part))))))

(defn partition2
  "Use flood fill to partition a space and return a list of partitions with their points.
  The decider-fn should take a point and a neighbour and return true/false accordingly if
  the neighbour is reachable."
  [points decider-fn x1 y1 x2 y2]
  (loop [remaining points
         i 0
         partitions '()]
    (if (= 0 (count remaining))
      partitions
      (let [seed (first remaining)
            testpoints (rest remaining)
            part (fill2 seed testpoints decider-fn x1 y1 x2 y2)
            restpoints (remove (set part) testpoints)
            ;tmp (dorun (println (str "run " i ": " (count part) " - " (count restpoints))))
            ]
        (recur restpoints (+ i 1) (conj partitions part))))))
