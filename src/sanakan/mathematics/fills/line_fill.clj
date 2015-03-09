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

(defn lineify
  "Map each line to a list of line segments that are acceptable."
  [p max-x max-y decider-fn]
  (map #(reduce-line (filter-line2 % max-x decider-fn)) (make-column p max-y)))

(defn overlaps?
  "Check if two lines overlap on the x axis."
  [l1 l2]
  (let [x11 (:x (:p1 l1))
        x12 (:x (:p2 l1))
        x21 (:x (:p1 l2))
        x22 (:x (:p2 l2))]
    (or
      (and (<= x11 x21) (<= x12 x22) (>= x12 x21))
      (and (<= x11 x21) (>= x12 x22))
      (and (>= x11 x21) (>= x12 x22) (<= x11 x22))
      (and (>= x11 x21) (<= x12 x22)))))

(defn find-matching-segments
  "Find matching line segments that overlap and are acceptable."
  [line lines decider-fn]
  (filter #(and (overlaps? line %) (decider-fn (:p1 line) (:p1 %))) lines))

(defn fill
  "Find clusters of line segments that are accepted by the decider function."
  [p max-x max-y decider-fn]
  (map #(identity) (lineify p max-x max-y decider-fn)))

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

