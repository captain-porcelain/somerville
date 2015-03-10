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

(defn complex-decider-fn
  [p1 p2]
  (if (and (< (:y p1) 2) (< (:y p2) 2))
    true
    (or
      (and (< (:x p1) 2) (< (:x p2) 2))
      (and (= (:x p1) 2) (= (:x p2) 2))
      (and (> (:x p1) 2) (> (:x p2) 2)))))

;; Verify that the test functions do as they should.
(fact (blocked-decider-fn (p/point -1 -1) (p/point -2 -2)) => true)
(fact (blocked-decider-fn (p/point -1 -1) (p/point  2  2)) => false)
(fact (blocked-decider-fn (p/point  2  2) (p/point  2  2)) => true)
(fact (blocked-decider-fn (p/point  2  2) (p/point  3  3)) => false)
(fact (blocked-decider-fn (p/point  3  3) (p/point  4  4)) => true)

(fact (complex-decider-fn (p/point  0  0) (p/point  1  1)) => true)
(fact (complex-decider-fn (p/point  0  0) (p/point  2  2)) => false)
(fact (complex-decider-fn (p/point  0  0) (p/point  3  2)) => false)
(fact (complex-decider-fn (p/point  0  0) (p/point  4  1)) => true)
(fact (complex-decider-fn (p/point  2  2) (p/point  2  2)) => true)
(fact (complex-decider-fn (p/point  2  2) (p/point  3  3)) => false)
(fact (complex-decider-fn (p/point  3  3) (p/point  4  4)) => true)

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
(fact (count         lined)        => 11)
(fact (count    (nth lined 0))     => 2)
(fact (:p1 (nth (nth lined 0) 0))  => (p/point -5 -5))
(fact (:p2 (nth (nth lined 0) 0))  => (p/point -1 -5))
(fact (:p1 (nth (nth lined 0) 1))  => (p/point  0 -5))
(fact (:p2 (nth (nth lined 0) 1))  => (p/point  5 -5))
(fact (count    (nth lined 4))     => 2)
(fact (:p1 (nth (nth lined 4) 0))  => (p/point -5 -1))
(fact (:p2 (nth (nth lined 4) 0))  => (p/point -1 -1))
(fact (:p1 (nth (nth lined 4) 1))  => (p/point  0 -1))
(fact (:p2 (nth (nth lined 4) 1))  => (p/point  5 -1))
(fact (count    (nth lined 5))     => 1)
(fact (:p1 (nth (nth lined 5) 0))  => (p/point -5  0))
(fact (:p2 (nth (nth lined 5) 0))  => (p/point  5  0))

;; Verify that we can identify line segments that overlap in x and beloge together by the decider function.
(fact (lf/overlaps? (l/line (p/point 1 0) (p/point 3 0)) (l/line (p/point 2 0) (p/point 4 0))) => true)
(fact (lf/overlaps? (l/line (p/point 1 0) (p/point 4 0)) (l/line (p/point 2 0) (p/point 3 0))) => true)
(fact (lf/overlaps? (l/line (p/point 2 0) (p/point 4 0)) (l/line (p/point 1 0) (p/point 3 0))) => true)
(fact (lf/overlaps? (l/line (p/point 2 0) (p/point 3 0)) (l/line (p/point 1 0) (p/point 4 0))) => true)
(fact (lf/overlaps? (l/line (p/point 1 0) (p/point 2 0)) (l/line (p/point 3 0) (p/point 4 0))) => false)
(fact (lf/overlaps? (l/line (p/point 3 0) (p/point 4 0)) (l/line (p/point 1 0) (p/point 2 0))) => false)

(def matched (lf/find-matching-segment (first (first lined)) (second lined) negative-decider-fn))
(fact matched => (l/line (p/point -5 -4) (p/point -1 -4)))

;; Test that growing a segment works.
(def matched2 (lf/attach-matching-segment (list (first (first lined))) (second lined) negative-decider-fn))
(fact (first  matched2) => (l/line (p/point -5 -4) (p/point -1 -4)))
(fact (second matched2) => (first (first lined)))

;; Test that partitioning works correctly.
(def parts (lf/partition (p/point -5 -5) 5 5 blocked-decider-fn))
(fact (count      parts)    => 3)
(fact (count (nth parts 0)) => 11)
(dorun
  (for [p (nth parts 0)]
    (let []
      (fact (:x (:p1 p)) => -5)
      (fact (:x (:p2 p)) =>  1))))
(fact (count (nth parts 1)) => 11)
(dorun
  (for [p (nth parts 1)]
    (let []
      (fact (:x (:p1 p)) =>  2)
      (fact (:x (:p2 p)) =>  2))))
(fact (count (nth parts 2)) => 11)
(dorun
  (for [p (nth parts 2)]
    (let []
      (fact (:x (:p1 p)) =>  3)
      (fact (:x (:p2 p)) =>  5))))

;; Test partitioning with changing amount of segments per line
(def parts2 (lf/partition (p/point 0 0) 5 5 complex-decider-fn))
(fact (count      parts2)      => 3)
(fact (count (nth parts2 0))   => 6)
(fact (nth   (nth parts2 0) 0) => (l/line (p/point 0 5) (p/point 1 5)))
(fact (nth   (nth parts2 0) 1) => (l/line (p/point 0 4) (p/point 1 4)))
(fact (nth   (nth parts2 0) 2) => (l/line (p/point 0 3) (p/point 1 3)))
(fact (nth   (nth parts2 0) 3) => (l/line (p/point 0 2) (p/point 1 2)))
(fact (nth   (nth parts2 0) 4) => (l/line (p/point 0 1) (p/point 5 1)))
(fact (nth   (nth parts2 0) 5) => (l/line (p/point 0 0) (p/point 5 0)))
(fact (count (nth parts2 1))   => 4)
(fact (nth   (nth parts2 1) 0) => (l/line (p/point 2 5) (p/point 2 5)))
(fact (nth   (nth parts2 1) 1) => (l/line (p/point 2 4) (p/point 2 4)))
(fact (nth   (nth parts2 1) 2) => (l/line (p/point 2 3) (p/point 2 3)))
(fact (nth   (nth parts2 1) 3) => (l/line (p/point 2 2) (p/point 2 2)))
(fact (count (nth parts2 2))   => 4)
(fact (nth   (nth parts2 2) 0) => (l/line (p/point 3 5) (p/point 5 5)))
(fact (nth   (nth parts2 2) 1) => (l/line (p/point 3 4) (p/point 5 4)))
(fact (nth   (nth parts2 2) 2) => (l/line (p/point 3 3) (p/point 5 3)))
(fact (nth   (nth parts2 2) 3) => (l/line (p/point 3 2) (p/point 5 2)))
