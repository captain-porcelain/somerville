(ns sanakan.mathematics.fills.line-fill-test
  (:require
    [sanakan.mathematics.fills.line-fill :as lf]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.point :as p])
  (:use midje.sweet))

(defn negative-decider-fn
  [p1 p2]
  (and (< (:x p2) 0) (< (:y p2) 0)))

(defn blocked-decider-fn
  [p1 p2]
  (or
    (and (< (:x p1) 2) (< (:x p2) 2))
    (and (= (:x p1) 2) (= (:x p2) 2))
    (and (> (:x p1) 2) (> (:x p2) 2))))

(fact (blocked-decider-fn (p/point -1 -1) (p/point -2 -2)) => true)
(fact (blocked-decider-fn (p/point -1 -1) (p/point  2  2)) => false)
(fact (blocked-decider-fn (p/point  2  2) (p/point  2  2)) => true)
(fact (blocked-decider-fn (p/point  2  2) (p/point  3  3)) => false)
(fact (blocked-decider-fn (p/point  3  3) (p/point  4  4)) => true)

(fact (count (lf/filter-line (p/point -5  0)  5 negative-decider-fn)) =>  0)
(fact (count (lf/filter-line (p/point -5 -1)  5 negative-decider-fn)) =>  5)
(fact (count (lf/filter-line (p/point -5 -1) -2 negative-decider-fn)) =>  4)

(fact (count (lf/make-line   (p/point 0 0) 10)) => 11)
(fact (count (lf/make-column (p/point 0 0) 10)) => 11)

(def seed (atom (p/point -3 -1)))
(fact ((lf/partition-fn negative-decider-fn seed) (p/point -2 -1)) => true)
(fact ((lf/partition-fn negative-decider-fn seed) (p/point -1 -1)) => true)
(fact ((lf/partition-fn negative-decider-fn seed) (p/point  0 -1)) => false)
(fact (= @seed (p/point 0 -1)))

(fact (count (lf/filter-line2 (p/point -5 -1) -2 negative-decider-fn)) =>  1)
(fact (count (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn)) =>  2)
(fact (count (nth (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn) 0)) =>  5)
(fact (count (nth (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn) 1)) =>  6)

(fact (count (lf/reduce-line (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn))) =>  2)
(dorun (println (lf/reduce-line (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn))))
(fact (:p1 (nth (lf/reduce-line (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn)) 0)) =>  (p/point -5 -1))
(fact (:p2 (nth (lf/reduce-line (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn)) 0)) =>  (p/point -1 -1))
(fact (:p1 (nth (lf/reduce-line (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn)) 1)) =>  (p/point  0 -1))
(fact (:p2 (nth (lf/reduce-line (lf/filter-line2 (p/point -5 -1)  5 negative-decider-fn)) 1)) =>  (p/point  5 -1))

(def p1 (p/point -5 -5))

(fact (count (lf/fill2 p1 5 5 negative-decider-fn)) => 11)
(fact (count (nth (lf/fill2 p1 5 5 negative-decider-fn) 0)) => 2)
(fact (:p1 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 0) 0))  => (p/point -5 -5))
(fact (:p2 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 0) 0))  => (p/point -1 -5))
(fact (:p1 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 0) 1))  => (p/point  0 -5))
(fact (:p2 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 0) 1))  => (p/point  5 -5))
(fact (count (nth (lf/fill2 p1 5 5 negative-decider-fn) 4)) => 2)
(fact (:p1 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 4) 0))  => (p/point -5 -1))
(fact (:p2 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 4) 0))  => (p/point -1 -1))
(fact (:p1 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 4) 1))  => (p/point  0 -1))
(fact (:p2 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 4) 1))  => (p/point  5 -1))
(fact (count (nth (lf/fill2 p1 5 5 negative-decider-fn) 5)) => 1)
(fact (:p1 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 5) 0))  => (p/point -5  0))
(fact (:p2 (nth (nth (lf/fill2 p1 5 5 negative-decider-fn) 5) 0))  => (p/point  5  0))

(fact (count (lf/filter-row  (p/point -5 -1) -2 negative-decider-fn)) =>  0)
(fact (count (lf/filter-row  (p/point -5 -5)  5 negative-decider-fn)) =>  5)

(fact (count (lf/filter-row  (p/point -5 -5)  5  blocked-decider-fn)) => 11)
(fact (count (lf/filter-line (p/point -5 -5)  5  blocked-decider-fn)) =>  7)

(fact (count (:points     (lf/fill p1 5 5 negative-decider-fn))) => 25)
(fact (count (:candidates (lf/fill p1 5 5 negative-decider-fn))) =>  5)
(fact (count (:points     (lf/fill p1 5 5  blocked-decider-fn))) => 77)
(fact (count (:candidates (lf/fill p1 5 5  blocked-decider-fn))) => 11)

;(fact (count (lf/partition blocked-decider-fn -5 -5 5 5)) => 3)
;(dorun (println (map #(count %) (lf/partition blocked-decider-fn -5 -5 5 5))))

;(dorun (println (str (java.util.Date.) " start")))
;(fact (count (lf/partition blocked-decider-fn -50 -50 50 50)) => 3)
;(dorun (println (str (java.util.Date.) " end")))
;(dorun (println (map #(c/out %) (last (lf/partition blocked-decider-fn -5 -5 5 5)))))
