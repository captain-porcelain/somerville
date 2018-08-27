(ns somerville.fills.line-fill-test
  (:require
    [somerville.fills.line-fill :as lf]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

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

(defn vertical-decider-fn
  [p1 p2]
  (or
    (and (< (:y p1) 2) (< (:y p2) 2))
    (and (= (:y p1) 2) (= (:y p2) 2))
    (and (> (:y p1) 2) (> (:y p2) 2))))

(defn complex-decider-fn
  [p1 p2]
  (if (and (< (:y p1) 2) (< (:y p2) 2))
    true
    (or
      (and (< (:x p1) 2) (< (:x p2) 2))
      (and (= (:x p1) 2) (= (:x p2) 2))
      (and (> (:x p1) 2) (> (:x p2) 2)))))

;; Verify that the test functions do as they should.
(deftest deciders
  (is (= (blocked-decider-fn (p/point -1 -1) (p/point -2 -2)) true))
  (is (= (blocked-decider-fn (p/point -1 -1) (p/point  2  2)) false))
  (is (= (blocked-decider-fn (p/point  2  2) (p/point  2  2)) true))
  (is (= (blocked-decider-fn (p/point  2  2) (p/point  3  3)) false))
  (is (= (blocked-decider-fn (p/point  3  3) (p/point  4  4)) true))

  (is (= (complex-decider-fn (p/point  0  0) (p/point  1  1)) true))
  (is (= (complex-decider-fn (p/point  0  0) (p/point  2  2)) false))
  (is (= (complex-decider-fn (p/point  0  0) (p/point  3  2)) false))
  (is (= (complex-decider-fn (p/point  0  0) (p/point  4  1)) true))
  (is (= (complex-decider-fn (p/point  2  2) (p/point  2  2)) true))
  (is (= (complex-decider-fn (p/point  2  2) (p/point  3  3)) false))
  (is (= (complex-decider-fn (p/point  3  3) (p/point  4  4)) true)))

;; Test that we can generate points for lines and columns.
(deftest generators
  (is (= (count (lf/make-line   (p/point 0 0) 10)) 11))
  (is (= (count (lf/make-column (p/point 0 0) 10)) 11)))

;; Verify that the internally used function to split based on the decider works.
;; It uses some state that it is provided to keep track of the current reference point.
(deftest splitting
  (let [seed (atom (p/point -3 -1))]
    (is (= ((lf/partition-fn negative-decider-fn seed) (p/point -2 -1)) true))
    (is (= ((lf/partition-fn negative-decider-fn seed) (p/point -1 -1)) true))
    (is (= ((lf/partition-fn negative-decider-fn seed) (p/point  0 -1)) false))
    ;; The reference point must be updated to the one the last change was found.
    (is (= @seed (p/point 0 -1)))

    ;; Test that we are able to break a line of points into lists of points by applying the decider functions.
    (is (= (count      (lf/filter-line (p/point -5 -1) -2 negative-decider-fn))     1))
    (is (= (count      (lf/filter-line (p/point -5 -1)  5 negative-decider-fn))     2))
    (is (= (count (nth (lf/filter-line (p/point -5 -1)  5 negative-decider-fn) 0))  5))
    (is (= (count (nth (lf/filter-line (p/point -5 -1)  5 negative-decider-fn) 1))  6))

    ;; Also make sure that we collapse the partitioned line into a list of line segments.
    (is (= (count    (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)))  2))
    (is (= (:p1 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 0))  (p/point -5 -1)))
    (is (= (:p2 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 0))  (p/point -1 -1)))
    (is (= (:p1 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 1))  (p/point  0 -1)))
    (is (= (:p2 (nth (lf/reduce-line (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) 1))  (p/point  5 -1)))))

;; Test that we can combine the functionality tested above into a an algorithm that returnes a list of lines
;; where each line consists of partitioned lines.
(deftest integration
  (let [p1 (p/point -5 -5)
        lined (lf/lineify p1 5 5 negative-decider-fn)
        matched (lf/select-line (first (first lined)) (second lined) negative-decider-fn)
        matched2 (lf/grow-cluster (list (first (first lined))) (second lined) negative-decider-fn)]
    (is (= (count         lined)        11))
    (is (= (count    (nth lined 0))     2))
    (is (= (:p1 (nth (nth lined 0) 0))  (p/point -5 -5)))
    (is (= (:p2 (nth (nth lined 0) 0))  (p/point -1 -5)))
    (is (= (:p1 (nth (nth lined 0) 1))  (p/point  0 -5)))
    (is (= (:p2 (nth (nth lined 0) 1))  (p/point  5 -5)))
    (is (= (count    (nth lined 4))     2))
    (is (= (:p1 (nth (nth lined 4) 0))  (p/point -5 -1)))
    (is (= (:p2 (nth (nth lined 4) 0))  (p/point -1 -1)))
    (is (= (:p1 (nth (nth lined 4) 1))  (p/point  0 -1)))
    (is (= (:p2 (nth (nth lined 4) 1))  (p/point  5 -1)))
    (is (= (count    (nth lined 5))     1))
    (is (= (:p1 (nth (nth lined 5) 0))  (p/point -5  0)))
    (is (= (:p2 (nth (nth lined 5) 0))  (p/point  5  0)))

    ;; Verify that we can identify line segments that overlap in x and beloge together by the decider function.
    (is (= (lf/overlaps? (l/line (p/point 1 0) (p/point 3 0)) (l/line (p/point 2 0) (p/point 4 0))) true))
    (is (= (lf/overlaps? (l/line (p/point 1 0) (p/point 4 0)) (l/line (p/point 2 0) (p/point 3 0))) true))
    (is (= (lf/overlaps? (l/line (p/point 2 0) (p/point 4 0)) (l/line (p/point 1 0) (p/point 3 0))) true))
    (is (= (lf/overlaps? (l/line (p/point 2 0) (p/point 3 0)) (l/line (p/point 1 0) (p/point 4 0))) true))
    (is (= (lf/overlaps? (l/line (p/point 1 0) (p/point 2 0)) (l/line (p/point 3 0) (p/point 4 0))) false))
    (is (= (lf/overlaps? (l/line (p/point 3 0) (p/point 4 0)) (l/line (p/point 1 0) (p/point 2 0))) false))

    (is (= matched (l/line (p/point -5 -4) (p/point -1 -4))))

    (is (= (first  matched2) (l/line (p/point -5 -4) (p/point -1 -4))))
    (is (= (second matched2) (first (first lined))))))

;; Test that partitioning works correctly.
(deftest partitioning
  (let [parts (lf/clusters (p/point -5 -5) 5 5 blocked-decider-fn)]
    (is (= (count      parts)    3))
    (is (= (count (nth parts 0)) 11))
    (dorun
      (for [p (nth parts 0)]
        (let []
          (is (= (:x (:p1 p)) -5))
          (is (= (:x (:p2 p))  1)))))
    (is (= (count (nth parts 1)) 11))
    (dorun
      (for [p (nth parts 1)]
        (let []
          (is (= (:x (:p1 p))  2))
          (is (= (:x (:p2 p))  2)))))
    (is (= (count (nth parts 2)) 11))
    (dorun
      (for [p (nth parts 2)]
        (let []
          (is (= (:x (:p1 p))  3))
          (is (= (:x (:p2 p))  5)))))))

;; Test partitioning with changing amount of segments per line
(deftest partition-changing-amounts
  (let [parts2 (lf/clusters (p/point 0 0) 5 5 complex-decider-fn)]
    (is (= (count      parts2)      3))
    (is (= (count (nth parts2 0))   6))
    (is (= (nth   (nth parts2 0) 0) (l/line (p/point 0 5) (p/point 1 5))))
    (is (= (nth   (nth parts2 0) 1) (l/line (p/point 0 4) (p/point 1 4))))
    (is (= (nth   (nth parts2 0) 2) (l/line (p/point 0 3) (p/point 1 3))))
    (is (= (nth   (nth parts2 0) 3) (l/line (p/point 0 2) (p/point 1 2))))
    (is (= (nth   (nth parts2 0) 4) (l/line (p/point 0 1) (p/point 5 1))))
    (is (= (nth   (nth parts2 0) 5) (l/line (p/point 0 0) (p/point 5 0))))
    (is (= (count (nth parts2 1))   4))
    (is (= (nth   (nth parts2 1) 0) (l/line (p/point 2 5) (p/point 2 5))))
    (is (= (nth   (nth parts2 1) 1) (l/line (p/point 2 4) (p/point 2 4))))
    (is (= (nth   (nth parts2 1) 2) (l/line (p/point 2 3) (p/point 2 3))))
    (is (= (nth   (nth parts2 1) 3) (l/line (p/point 2 2) (p/point 2 2))))
    (is (= (count (nth parts2 2))   4))
    (is (= (nth   (nth parts2 2) 0) (l/line (p/point 3 5) (p/point 5 5))))
    (is (= (nth   (nth parts2 2) 1) (l/line (p/point 3 4) (p/point 5 4))))
    (is (= (nth   (nth parts2 2) 2) (l/line (p/point 3 3) (p/point 5 3))))
    (is (= (nth   (nth parts2 2) 3) (l/line (p/point 3 2) (p/point 5 2))))))

;; Test that partitioning works when clusters end
(deftest partition-on-clusters-end
  (let [parts3 (lf/clusters (p/point -5 -5) 5 5 vertical-decider-fn)]
    (is (= (count      parts3)      3))
    (is (= (count (nth parts3 0))   7))
    (is (= (nth   (nth parts3 0) 0) (l/line (p/point -5  1) (p/point 5  1))))
    (is (= (nth   (nth parts3 0) 1) (l/line (p/point -5  0) (p/point 5  0))))
    (is (= (nth   (nth parts3 0) 2) (l/line (p/point -5 -1) (p/point 5 -1))))
    (is (= (nth   (nth parts3 0) 3) (l/line (p/point -5 -2) (p/point 5 -2))))
    (is (= (nth   (nth parts3 0) 4) (l/line (p/point -5 -3) (p/point 5 -3))))
    (is (= (nth   (nth parts3 0) 5) (l/line (p/point -5 -4) (p/point 5 -4))))
    (is (= (nth   (nth parts3 0) 6) (l/line (p/point -5 -5) (p/point 5 -5))))
    (is (= (count (nth parts3 1))   1))
    (is (= (nth   (nth parts3 1) 0) (l/line (p/point -5  2) (p/point 5  2))))
    (is (= (count (nth parts3 2))   3))
    (is (= (nth   (nth parts3 2) 0) (l/line (p/point -5  5) (p/point 5  5))))
    (is (= (nth   (nth parts3 2) 1) (l/line (p/point -5  4) (p/point 5  4))))
    (is (= (nth   (nth parts3 2) 2) (l/line (p/point -5  3) (p/point 5  3))))))

;; Test helper functions on clusters
(deftest helpers
  (let [parts2 (lf/clusters (p/point 0 0) 5 5 complex-decider-fn)
        parts3 (lf/clusters (p/point -5 -5) 5 5 vertical-decider-fn)]
    (is (= (lf/cluster-size nil)             0))
    (is (= (lf/cluster-size (nth parts3 0)) 77))
    (is (= (lf/cluster-size (nth parts3 1)) 11))
    (is (= (lf/cluster-size (nth parts3 2)) 33))

    (is (= (lf/in-line? (p/point 0 0) (l/line (p/point 1 1) (p/point 3 1))) false))
    (is (= (lf/in-line? (p/point 0 1) (l/line (p/point 1 1) (p/point 3 1))) false))
    (is (= (lf/in-line? (p/point 1 1) (l/line (p/point 1 1) (p/point 3 1))) true))
    (is (= (lf/in-line? (p/point 3 1) (l/line (p/point 1 1) (p/point 3 1))) true))
    (is (= (lf/in-line? (p/point 4 1) (l/line (p/point 1 1) (p/point 3 1))) false))

    (is (= (lf/in-cluster? (p/point -10 -10) (nth parts3 0)) false))
    (is (= (lf/in-cluster? (p/point   0   0) (nth parts3 0)) true))

    (is (= (lf/line-weight (nth (nth parts3 0) 0)) {:wx 0 :wy 11}))
    (is (= (lf/line-weight (nth (nth parts3 1) 0)) {:wx 0 :wy 22}))
    (is (= (lf/line-weight (nth (nth parts2 1) 0)) {:wx 2 :wy 5}))
    (is (= (lf/line-weight (nth (nth parts2 2) 0)) {:wx 12 :wy 15}))

    (is (= (lf/cluster-center (nth parts3 0)) (p/point 0 -2)))
    (is (= (lf/cluster-center (nth parts3 1)) (p/point 0  2)))
    (is (= (lf/cluster-center (nth parts3 2)) (p/point 0  4)))
    (is (= (lf/cluster-center (nth parts2 0)) (p/point 1  1)))))
