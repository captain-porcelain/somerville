(ns somerville.fills.concave-hull-test
  (:require
    [somerville.fills.concave-hull :as ch]
    [somerville.geometry.commons :as c]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p])
  (:use [clojure.test]))

(def p1  (p/point -1  1))
(def p2  (p/point  0  1))
(def p3  (p/point  1  1))
(def p4  (p/point -2  0))
(def p5  (p/point -1  0))
(def p6  (p/point  0  0))
(def p7  (p/point  1  0))
(def p8  (p/point  2  0))
(def p9  (p/point -1 -1))
(def p10 (p/point  0 -1))
(def p11 (p/point  1 -1))
(def points (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11))

;
;           |
;       1   2   3
;           |
; - 4 - 5 - 6 - 7 - 8 -
;           |
;       9   10  11
;           |
;

(deftest neighbors
  (let [kn (ch/k-nearest-neighbors 3 p1 (remove #{p1} points))]
    (is (= p2 (nth kn 0)))
    (is (= p5 (nth kn 1)))
    (is (= p4 (nth kn 2)))))

(deftest hull
  (let [h (ch/hull points 3)]
    (is (= 8 (count (:lines h))))
    (is (= p9  (:p1 (nth (:lines h) 0))))
    (is (= p4  (:p2 (nth (:lines h) 0))))
    (is (= p4  (:p1 (nth (:lines h) 1))))
    (is (= p1  (:p2 (nth (:lines h) 1))))
    (is (= p1  (:p1 (nth (:lines h) 2))))
    (is (= p2  (:p2 (nth (:lines h) 2))))
    (is (= p2  (:p1 (nth (:lines h) 3))))
    (is (= p3  (:p2 (nth (:lines h) 3))))
    (is (= p3  (:p1 (nth (:lines h) 4))))
    (is (= p8  (:p2 (nth (:lines h) 4))))
    (is (= p8  (:p1 (nth (:lines h) 5))))
    (is (= p11 (:p2 (nth (:lines h) 5))))
    (is (= p11 (:p1 (nth (:lines h) 6))))
    (is (= p10 (:p2 (nth (:lines h) 6))))
    (is (= p10 (:p1 (nth (:lines h) 7))))
    (is (= p9  (:p2 (nth (:lines h) 7))))))
