;; Provides a simple algorithm to flood fill an area
(ns sanakan.mathematics.flood-fill)

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

(defn add-value
  "Update all points with their value."
  [points value-fn]
  (map #(assoc % :value (value-fn %)) points))

(defn test-neighbours
  "Test all neighbours and return those that should be added."
  [point points decider-fn]
  (let [n (neighbours points point)]
    (filter #(decider-fn point %) n)))

(defn fill
  "Start with the first point in the list of points and take as long as
  the decider function returns true for the results of the value function
  of the current point and the next one.
  The value-fn should take a Point and return some value.
  The decider-fn should take a point with a value and a neighbour with
  value and return true/false accordingly if the neighbour is reachable."
  [seed points value-fn decider-fn]
  (let [testpoints (add-value points value-fn)] ;; add values to all points
    (loop [newpoints (list (assoc seed :value (value-fn seed))) ;; put seed into candidates list
           accepted '()]
      (if (= 0 (count newpoints))
        accepted
        (let [p (first newpoints) ;; take seed
              n (test-neighbours p testpoints decider-fn) ;; and get fitting neighbours
              np (concat (rest newpoints) (remove (set (concat newpoints accepted)) n)) ;; filter neighbours to new ones
              ap (conj accepted p)]
          (recur np ap))))))

(defn partition
  "Use flood fill to partition a space and return a list of partitions with their points."
  [points value-fn decider-fn]
  (loop [remaining (add-value points value-fn)
         i 0
         partitions '()]
    (if (= 0 (count remaining))
      partitions
      (let [seed (first remaining)
            testpoints (rest remaining)
            part (fill seed testpoints value-fn decider-fn)
            restpoints (remove (set part) testpoints)
            ;tmp (dorun (println (str "run " i ": " (count part) " - " (count restpoints))))
            ]
        (recur restpoints (+ i 1) (conj partitions part))))))
