(ns sanakan.mathematics.flood-fill-test
  (:require
    [sanakan.mathematics.flood-fill :as ff]
    [sanakan.mathematics.geometry.point :as p])
  (:use midje.sweet))

(fact (ff/neighbour? (p/point 0 0) (p/point  1  1)) => true)
(fact (ff/neighbour? (p/point 0 0) (p/point  2  1)) => false)
(fact (ff/neighbour? (p/point 0 0) (p/point -1  1)) => true)
(fact (ff/neighbour? (p/point 0 0) (p/point  1 -2)) => false)

(def numbers (take 11 (iterate inc -5)))
(def points (for [a numbers
                  b numbers]
              (p/point a b)))

(def neighbours-of-zero
  (ff/neighbours points (p/point 0 0)))

(fact (count neighbours-of-zero) => 8)

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
(fact (count (ff/test-neighbours p1 points negative-decider-fn)) => 3)

(def p2 (p/point 0 0))
(fact (count (ff/test-neighbours p2 points negative-decider-fn)) => 1)

(def p3 (p/point -2 -2))
(fact (count (ff/test-neighbours p3 points negative-decider-fn)) => 8)

(fact (ff/in-bounds? p1 -10 -10 10 10) => true)
(fact (ff/in-bounds? p1  -1  -1 10 10) => true)
(fact (ff/in-bounds? p1   0   0 10 10) => false)

(fact (count (ff/test-neighbours2 p1 negative-decider-fn -10 -10 10 10)) => 3)
(fact (count (ff/test-neighbours2 p2 negative-decider-fn -10 -10 10 10)) => 1)
(fact (count (ff/test-neighbours2 p3 negative-decider-fn -10 -10 10 10)) => 8)

(fact (count (ff/fill p1 points negative-decider-fn)) => 25)
(fact (count (ff/fill p1 points blocked-decider-fn)) => 77)

(fact (count (ff/fill2 p1 points negative-decider-fn -5 -5 5 5)) => 25)
(fact (count (ff/fill2 p1 points blocked-decider-fn -5 -5 5 5)) => 77)

(fact (count (ff/partition points negative-decider-fn)) => 97)
(fact (count (ff/partition points blocked-decider-fn)) => 3)

(fact (count (ff/partition2 points negative-decider-fn -5 -5 5 5)) => 97)
(fact (count (ff/partition2 points blocked-decider-fn -5 -5 5 5)) => 3)

(dorun (println "after testing first partitions"))

(defn test-get-neighbours
  [size]
  (let [more-numbers (take size (iterate inc (* -1 (/ size 2))))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/neighbours more-points p3))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "getting neighbours " size " took " (- endtime starttime) "ms")))))

(test-get-neighbours 10)
(test-get-neighbours 20)
(test-get-neighbours 30)
(test-get-neighbours 50)
(test-get-neighbours 320)

(defn test-filter1-neighbours
  [size]
  (let [more-numbers (take size (iterate inc (* -1 (/ size 2))))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/test-neighbours p3 (ff/add-value more-points negative-value-fn) negative-decider-fn))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "fitering neighbours old " size " took " (- endtime starttime) "ms")))))

(test-filter1-neighbours 10)
(test-filter1-neighbours 20)
(test-filter1-neighbours 50)
(test-filter1-neighbours 320)

(defn test-filter2-neighbours
  [size]
  (let [starttime (System/currentTimeMillis)
        parts (count (ff/test-neighbours2 p3 negative-decider-fn -10 -10 10 10))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "fitering neighbours new " size " took " (- endtime starttime) "ms")))))

(test-filter2-neighbours 10)
(test-filter2-neighbours 20)
(test-filter2-neighbours 50)
(test-filter2-neighbours 320)

(defn test-fill1
  [size]
  (let [x1 (* -1 (/ size 2))
        x2 (/ size 2)
        more-numbers (take size (iterate inc x1))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/fill (p/point x1 x1) more-points blocked-decider-fn))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "filling 1 " size "x" size " to " parts " took " (- endtime starttime) "ms")))))

(test-fill1 10)
(test-fill1 20)
(test-fill1 30)

(defn test-fill2
  [size]
  (let [x1 (* -1 (/ size 2))
        x2 (/ size 2)
        more-numbers (take size (iterate inc x1))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/fill2 (p/point x1 x1) more-points blocked-decider-fn x1 x1 x2 x2))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "filling 2 " size "x" size " to " parts " took " (- endtime starttime) "ms")))))

(test-fill2 10)
(test-fill2 20)
(test-fill2 30)

(defn test-partition1
  [size]
  (let [x1 (* -1 (/ size 2))
        x2 (/ size 2)
        more-numbers (take size (iterate inc x1))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/partition more-points blocked-decider-fn))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "partitioning 1 " size "x" size " to " parts " took " (- endtime starttime) "ms")))))

(test-partition1 10)
(test-partition1 20)
;(test-partition 30)

(defn test-partition2
  [size]
  (let [x1 (* -1 (/ size 2))
        x2 (/ size 2)
        more-numbers (take size (iterate inc x1))
        more-points (for [a more-numbers
                          b more-numbers]
                      (p/point a b))
        starttime (System/currentTimeMillis)
        parts (count (ff/partition2 more-points blocked-decider-fn x1 x1 x2 x2))
        endtime (System/currentTimeMillis)]
    (dorun (println (str "partitioning 2 " size "x" size " to " parts " took " (- endtime starttime) "ms")))))

(test-partition2 10)
(test-partition2 20)
;(test-partition 30)


