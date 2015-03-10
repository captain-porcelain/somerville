(ns sanakan.mathematics.fills.flood-fill-test
  (:require
    [sanakan.mathematics.fills.flood-fill :as ff]
    [sanakan.mathematics.geometry.point :as p])
  (:use midje.sweet))

(def numbers (take 11 (iterate inc -5)))
(def points (for [a numbers
                  b numbers]
              (p/point a b)))

(defn negative-value-fn
  [p]
  (and (< (:x p) 0) (< (:y p) 0)))

(defn negative-decider-fn
  [p1 p2]
  (and (< (:x p2) 0) (< (:y p2) 0)))

(defn blocked-value-fn
  [p]
  (if (= (:x p) 2) 100 (:x p)))

(defn blocked-decider-fn
  [p1 p2]
  (let [vfn (fn [p] (if (= (:x p) 2) 100 (:x p)))
        v1 (vfn p1)
        v2 (vfn p2)]
    (and (< -2 (- v1 v2)) (> 2 (- v1 v2)))))

(fact (blocked-decider-fn (p/point 1 0) (p/point 0 0)) => true)
(fact (blocked-decider-fn (p/point 1 0) (p/point 2 0)) => false)
(fact (blocked-decider-fn (p/point 2 0) (p/point 3 0)) => false)

(def p1 (p/point -1 -1))
(fact (count (ff/test-neighbours p1 negative-decider-fn -10 -10 10 10)) => 3)

(def p2 (p/point 0 0))
(fact (count (ff/test-neighbours p2 negative-decider-fn -10 -10 10 10)) => 1)

(def p3 (p/point -2 -2))
(fact (count (ff/test-neighbours p3 negative-decider-fn -10 -10 10 10)) => 8)

(fact (ff/in-bounds? p1 -10 -10 10 10) => true)
(fact (ff/in-bounds? p1  -1  -1 10 10) => true)
(fact (ff/in-bounds? p1   0   0 10 10) => false)

(fact (count (ff/fill p1 points negative-decider-fn -5 -5 5 5)) => 25)
(fact (count (ff/fill p1 points blocked-decider-fn -5 -5 5 5)) => 77)

(fact (count (ff/partition points negative-decider-fn -5 -5 5 5)) => 97)
(fact (count (ff/partition points blocked-decider-fn -5 -5 5 5)) => 3)

(defn test-fill
  [size]
  (let [x1 (* -1 (/ size 2))
        x2 (/ size 2)
        more-numbers (take size (iterate inc x1))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/fill (p/point x1 x1) more-points blocked-decider-fn x1 x1 x2 x2))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "filling " size "x" size " to " parts " took " (- endtime starttime) "ms")))))

;(test-fill 10)
;(test-fill 20)
;(test-fill 30)
;(test-fill 100)

(defn test-partition
  [size]
  (let [x1 (* -1 (/ size 2))
        x2 (/ size 2)
        more-numbers (take size (iterate inc x1))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/partition more-points blocked-decider-fn x1 x1 x2 x2))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "partitioning " size "x" size " to " parts " took " (- endtime starttime) "ms")))))

;(test-partition 10)
;(test-partition 20)
;(test-partition 30)
;(test-partition 100)

