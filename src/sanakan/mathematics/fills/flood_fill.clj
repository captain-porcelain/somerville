;; Provides a simple algorithm to flood fill an area
(ns sanakan.mathematics.fills.flood-fill
  (:require
    [sanakan.mathematics.geometry.point :as p]))

(defn neighbours
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

(defn in-bounds?
  "Check if a pixel is inside an image."
  [p x1 y1 x2 y2]
  (and
    (<= (:x p) x2)
    (>= (:x p) x1)
    (<= (:y p) y2)
    (>= (:y p) y1)))

(defn is-not-in
  [itm itms]
  (nil? (some #(= itm %) itms)))

(defn test-neighbours
  "Test all neighbours and return those that should be added."
  [point decider-fn x1 y1 x2 y2]
  (filter #(decider-fn point %) (filter #(in-bounds? % x1 y1 x2 y2) (neighbours point))))

(defn test-neighbours3
  "Test all neighbours and return those that should be added."
  [point decider-fn known-candidates accepted x1 y1 x2 y2]
  (filter #(is-not-in % accepted)
          (filter #(is-not-in % known-candidates)
                  (filter #(decider-fn point %)
                          (filter #(in-bounds? % x1 y1 x2 y2)
                                  (neighbours point))))))

(defn fill
  "Start with the first point in the list of points and take as long as
  the decider function returns true for the results of the value function
  of the current point and the next one."
  [seed points decider-fn x1 y1 x2 y2]
  (loop [newpoints (list seed) ;; put seed into candidates list
         i 0
         accepted '()]
    (if (= 0 (count newpoints))
      accepted
      (let [
            ;starttime (System/currentTimeMillis)
            p (first newpoints) ;; take seed
            n (test-neighbours p decider-fn x1 y1 x2 y2) ;; and get fitting neighbours
            n (filter #(is-not-in % accepted) (filter #(is-not-in % newpoints) n)) ;; remove unneeded ones
            np (concat (rest newpoints) n) ;; and add to list of candidates
            ap (conj accepted p)
            ;endtime (System/currentTimeMillis)
            ;tmp (dorun (println (str "fill run " i ": " (count np) " - " (count ap) " took " (- endtime starttime))))
            ]
        (recur np (+ i 1) ap)))))

(defn partition
  "Use flood fill to partition a space and return a list of partitions with their points.
  The decider-fn should take a point and a neighbour and return true/false accordingly if
  the neighbour is reachable."
  [points decider-fn x1 y1 x2 y2]
  (loop [remaining points
         i 0
         partitions '()]
    (if (= 0 (count remaining))
      partitions
      (let [
            ;starttime (System/currentTimeMillis)
            seed (first remaining)
            testpoints (rest remaining)
            part (fill seed testpoints decider-fn x1 y1 x2 y2)
            restpoints (remove (set part) testpoints)
            ;endtime (System/currentTimeMillis)
            ;tmp (dorun (println (str "partition run " i ": " (count part) " - " (count restpoints) " took " (- endtime starttime))))
            ]
        (recur restpoints (+ i 1) (conj partitions part))))))
