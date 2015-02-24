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
  [v nv]
  (:value nv))

(defn blocked-value-fn
  [p]
  (if (= (:x p) 2) 100 (:x p)))

(defn blocked-decider-fn
  [v nv]
  (and (< -2 (- (:value v) (:value nv))) (> 2 (- (:value v) (:value nv)))))

(fact (blocked-decider-fn {:value 1} {:value 2}) => true)
(fact (blocked-decider-fn {:value 100} {:value 2}) => false)
(fact (blocked-decider-fn {:value 2} {:value 100}) => false)

(def neg-v-points (ff/add-value points negative-value-fn))

(def p1 (p/point -1 -1))
(def neg-v-p1 (assoc p1 :value (negative-value-fn p1)))
(fact (count (ff/test-neighbours neg-v-p1 neg-v-points negative-decider-fn)) => 3)

(def p2 (p/point 0 0))
(def neg-v-p2 (assoc p2 :value (negative-value-fn p2)))
(fact (count (ff/test-neighbours neg-v-p2 neg-v-points negative-decider-fn)) => 1)

(def p3 (p/point -2 -2))
(def neg-v-p3 (assoc p3 :value (negative-value-fn p3)))
(fact (count (ff/test-neighbours neg-v-p3 neg-v-points negative-decider-fn)) => 8)

(fact (count (ff/fill (p/point -1 -1) points negative-value-fn negative-decider-fn)) => 25)

(fact (count (ff/fill (p/point -1 -1) points blocked-value-fn blocked-decider-fn)) => 77)


