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
  "Create a function that can be used to partition one line into segments."
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

(defn filter-line
  "Take points of a line while decider-fn accepts the points."
  [p max-x decider-fn]
  (let [seed (atom p)]
    (partition-by
      #((partition-fn decider-fn seed) %)
      (make-line p max-x))))

(defn reduce-line
  "Reduce the points of a list of lines to a list of line segments."
  [line]
  (map #(l/line (first %) (last %)) line))

(defn lineify
  "Create lines from p to max-x and max-y and map each line to a list of line segments that are acceptable."
  [p max-x max-y decider-fn]
  (map
    #(reduce-line
       (filter-line % max-x decider-fn))
    (make-column p max-y)))

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

(defn select-line
  "Find first matching line from candidates that overlap line and are accepted by the decider-fn."
  [line candidates decider-fn]
  (first
    (filter
      #(and
         (overlaps? line %)
         (decider-fn (:p1 line) (:p1 %)))
      candidates)))

(defn grow-cluster
  "From a list of candidates select the matching one and add it to a cluster."
  [cluster lines decider-fn]
  (let [matched (select-line (first cluster) lines decider-fn)]
    (if (nil? matched)
      cluster
      (conj cluster matched))))

(defn grow-clusters-old
  "Add lines to clusters if they match or create new clusters from them otherwise."
  [clusters lines decider-fn]
  (let [updated (map #(grow-cluster % lines decider-fn) clusters)
        matched (map first updated)
        unmatched (filter #(is-not-in % matched) lines)]
    (concat updated (map #(list %) unmatched))))

(defn grow-clusters
  "Add lines to clusters if they match or create new clusters from them otherwise."
  [clusters lines decider-fn]
  (loop [updated-clusters (list)
         remaining-clusters clusters
         candidates lines]
    (if (= 0 (count remaining-clusters))
      (concat updated-clusters (map #(list %) candidates))
      (let [updated-cluster (grow-cluster (first remaining-clusters) candidates decider-fn)
            found (first updated-cluster)]
        (recur (conj updated-clusters updated-cluster) (rest remaining-clusters) (remove #(= found %) candidates))))))

(defn cluster-size
  "Calculate size of cluster in points."
  [c]
  (reduce + (map #(+ 1 (- (:x (:p2 %)) (:x (:p1 %)))) c)))

(defn clusters
  "Find clusters of line segments that are accepted by the decider function."
  [p max-x max-y decider-fn]
  (let [lined (lineify p max-x max-y decider-fn)]
    (loop [clusters (map #(list %) (first lined))
           candidates (rest lined)]
      (if (= 0 (count candidates))
        clusters
        (recur (grow-clusters clusters (first candidates) decider-fn) (rest candidates))))))

(defn in-line?
  "Check if a point is in a line."
  [p line]
  (and
    (=  (:y p) (:y (:p1 line)))
    (>= (:x p) (:x (:p1 line)))
    (<= (:x p) (:x (:p2 line)))))

(defn in-cluster?
  "Check if a point is in a cluster."
  [p cluster]
  (= true (some true? (map #(in-line? p %) cluster))))

(defn line-weight
  "Calculate weight of one line."
  [line]
  (let [length (+ 1 (- (:x (:p2 line)) (:x (:p1 line))))
        wx (reduce + (take length (iterate inc (:x (:p1 line)))))
        wy (* length (:y (:p1 line)))]
    {:wx wx :wy wy}))

(defn cluster-center
  "Calculate center point for a cluster."
  [cluster]
  (let [weights (map line-weight cluster)
        wx (reduce + (map :wx weights))
        wy (reduce + (map :wy weights))
        c (cluster-size cluster)]
    (p/point (int (/ wx c)) (int (/ wy c)))))
