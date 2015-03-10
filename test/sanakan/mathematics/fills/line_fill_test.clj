(ns sanakan.mathematics.fills.line-fill-test
  (:require
    [sanakan.mathematics.fills.line-fill :as lf]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; Define decider functions for testing.
;; These functions will be used in the line fill algorithms.
(defn negative-decider-fn
  [p1 p2]
  (and (< (:x p2) 0) (< (:y p2) 0)))

(defn blocked-decider-fn
  [p1 p2]
  (or
    (and (< (:x p1) 2) (< (:x p2) 2))
    (and (= (:x p1) 2) (= (:x p2) 2))
    (and (> (:x p1) 2) (> (:x p2) 2))))

;; Verify that the test functions do as they should.
(fact (blocked-decider-fn (p/point -1 -1) (p/point -2 -2)) => true)
(fact (blocked-decider-fn (p/point -1 -1) (p/point  2  2)) => false)
(fact (blocked-decider-fn (p/point  2  2) (p/point  2  2)) => true)
(fact (blocked-decider-fn (p/point  2  2) (p/point  3  3)) => false)
(fact (blocked-decider-fn (p/point  3  3) (p/point  4  4)) => true)

;; Test that we can generate points for lines and columns.
(fact (count (lf/make-line   (p/point 0 0) 10)) => 11)
(fact (count (lf/make-column (p/point 0 0) 10)) => 11)

;; Verify that the internally used function to split based on the decider works.
;; It uses some state that it is provided to keep track of the current reference point.
(def seed (atom (p/point -3 -1)))
(fact ((lf/partition-fn negative-decider-fn seed) (p/point -2 -1)) => true)
(fact ((lf/partition-fn negative-decider-fn seed) (p/point -1 -1)) => true)
(fact ((lf/partition-fn negative-decider-fn seed) (p/point  0 -1)) => false)
;; The reference point must be updated to the one the last change was found.
(fact (= @seed (p/point 0 -1)))

;; Test that we are able to break a line of points into lists of points by applying the decider functions.
(fact (count      (lf/filter-line (p/point -5 -1) -2 negative-decider-fn))    =>  1)
(fact (count      (lf/filter-line (p/point -5 -1)  5 negative-decider-fn))    =>  2)
(fact (count (nth (lf/filter-line (p/point -5 -1)  5 negative-decider-fn) 0)) =>  5)
(fact (count (nth (lf/filter-line (p/point -5 -1)  5 negative-decider-fn) 1)) =>  6)

;; Also make sure that we collapse the partitioned line into a list of line segments.
(fact (count    (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn))) =>  2)
(fact (:p1 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 0)) =>  (p/point -5 -1))
(fact (:p2 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 0)) =>  (p/point -1 -1))
(fact (:p1 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 1)) =>  (p/point  0 -1))
(fact (:p2 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 1)) =>  (p/point  5 -1))

;; Test that we can combine the functionality tested above into a an algorithm that returnes a list of lines
;; where each line consists of partitioned lines.
(def p1 (p/point -5 -5))
(def lined (lf/lineify p1 5 5 negative-decider-fn))
(fact (count lined) => 11)
(fact (count (nth lined 0)) => 2)
(fact (:p1 (nth (nth lined 0) 0))  => (p/point -5 -5))
(fact (:p2 (nth (nth lined 0) 0))  => (p/point -1 -5))
(fact (:p1 (nth (nth lined 0) 1))  => (p/point  0 -5))
(fact (:p2 (nth (nth lined 0) 1))  => (p/point  5 -5))
(fact (count (nth lined 4)) => 2)
(fact (:p1 (nth (nth lined 4) 0))  => (p/point -5 -1))
(fact (:p2 (nth (nth lined 4) 0))  => (p/point -1 -1))
(fact (:p1 (nth (nth lined 4) 1))  => (p/point  0 -1))
(fact (:p2 (nth (nth lined 4) 1))  => (p/point  5 -1))
(fact (count (nth lined 5)) => 1)
(fact (:p1 (nth (nth lined 5) 0))  => (p/point -5  0))
(fact (:p2 (nth (nth lined 5) 0))  => (p/point  5  0))

;; Verify that we can identify line segments that overlap in x and beloge together by the decider function.
(fact (lf/overlaps? (l/line (p/point 1 0) (p/point 3 0)) (l/line (p/point 2 0) (p/point 4 0))) => true)
(fact (lf/overlaps? (l/line (p/point 1 0) (p/point 4 0)) (l/line (p/point 2 0) (p/point 3 0))) => true)
(fact (lf/overlaps? (l/line (p/point 2 0) (p/point 4 0)) (l/line (p/point 1 0) (p/point 3 0))) => true)
(fact (lf/overlaps? (l/line (p/point 2 0) (p/point 3 0)) (l/line (p/point 1 0) (p/point 4 0))) => true)
(fact (lf/overlaps? (l/line (p/point 1 0) (p/point 2 0)) (l/line (p/point 3 0) (p/point 4 0))) => false)
(fact (lf/overlaps? (l/line (p/point 3 0) (p/point 4 0)) (l/line (p/point 1 0) (p/point 2 0))) => false)

(def matched (lf/find-matching-segments (first (first lined)) (second lined) negative-decider-fn))
(fact matched => (list (l/line (p/point -5 -4) (p/point -1 -4))))

;(fact (count (lf/partition blocked-decider-fn -5 -5 5 5)) => 3)
;(dorun (println (map #(count %) (lf/partition blocked-decider-fn -5 -5 5 5))))

;(dorun (println (str (java.util.Date.) " start")))
;(fact (count (lf/partition blocked-decider-fn -50 -50 50 50)) => 3)
;(dorun (println (str (java.util.Date.) " end")))
;(dorun (println (map #(c/out %) (last (lf/partition blocked-decider-fn -5 -5 5 5)))))
