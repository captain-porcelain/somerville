;; Provides a simple algorithm to flood fill an area
(ns sanakan.mathematics.fills.line-fill
  (:require
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l]))

(defn is-not-in
  "Check that itm is not in itms."
  [itm itms]
  (nil? (some #(= itm %) itms)))

(defn partition-fn
  [decider-fn seed]
  (fn [p]
    (let [r (decider-fn @seed p)
          tmp (if r nil (reset! seed p))]
      r)))

(defn make-line
  "Given a point make a line with all following points up to the max value."
  [p max-x]
  (map #(p/point % (:y p)) (take-while #(<= % max-x) (iterate inc (:x p)))))

(defn make-column
  "Given a point make a column with all following points up to the max value."
  [p max-y]
  (map #(p/point (:x p) %) (take-while #(<= % max-y) (iterate inc (:y p)))))

(defn filter-line2
  "Take points of a line while decider-fn accepts the points."
  [p max-x decider-fn]
  (let [seed (atom p)]
    (partition-by
      #((partition-fn decider-fn seed) %)
      (make-line p max-x))))

(defn reduce-line
  "Reduce the points of a line to a list of line segments."
  [line]
  (map #(l/line (first %) (last %)) line))

(defn filter-line
  "Take points of a line while decider-fn accepts the points."
  [p max-x decider-fn]
  (take-while
    #(and (<= (:x %) max-x) (decider-fn p %))
    (map #(p/point % (:y p))
         (iterate inc (:x p)))))

(defn filter-row
  "Take points of a row while decider-fn accepts the points."
  [p max-y decider-fn]
  (take-while
    #(and (<= (:y %) max-y) (decider-fn p %))
    (map #(p/point (:x p) %)
         (iterate inc (:y p)))))

(defn fill2
  "Start at a point and "
  [p max-x max-y decider-fn]
  (map #(reduce-line (filter-line2 % max-x decider-fn)) (make-column p max-y)))

(defn fill
  "Start at a point and "
  [p max-x max-y decider-fn]
  (let [ys (filter-row p max-y decider-fn)
        xs (map #(filter-line % max-x decider-fn) ys)
        last-points (map last xs)
        candidates (map
                     #(p/point
                        (+ (:x %) 1)
                        (:y %))
                     (filter #(not (nil? %)) last-points))]
    {:points (reduce concat xs) :candidates candidates}))

(defn partition
  ""
  [decider-fn x1 y1 max-x max-y]
  (loop [candidates (list (p/point x1 y1))
         partitions '()]
    (if (= 0 (count candidates))
      partitions
      (let [p (first candidates)
            filled (fill p max-y max-y decider-fn)
            r (:candidates filled)
            ;r (filter #(is-not-in % all) (:candidates filled))
            c (concat r (rest candidates))
            parts (conj partitions (:points filled))
            ;a (concat all (:points filled))
            ]
        (recur c parts)))))

