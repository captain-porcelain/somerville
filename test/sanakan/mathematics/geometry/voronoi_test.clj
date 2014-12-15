(ns sanakan.mathematics.geometry.voronoi-test
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.voronoi :as v]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; test bisector helper functions
(def p1 (p/point 1 1))
(def p2 (p/point 3 7))
(def points1 (list p1 p2))
(def bisectors1 (v/bisectors points1))
(fact (count bisectors1) => 2)
(fact (count (:bisectors (first bisectors1))) => 1)
(fact (:point (first bisectors1)) => p1)
(fact (count (:bisectors (second bisectors1))) => 1)
(fact (:point (second bisectors1)) => p2)

;; test properties of calculated voronois
(def p3 (p/point 4 5))
(def p4 (p/point 10 9))
(def p5 (p/point 2 10))
(def p6 (p/point 5 1))
(def points2 (list p3 p4 p5 p6))
(def v1 (v/voronoi points2 0 0 12 12))
(dorun (println (map #(str (c/out (:intersection %)) "\n") (:intersections (first (:points v1))))))
(dorun (println (map #(str (c/out %)) (v/cell-corners (first (:points v1))))))

(fact (count (:intersections (v/intersect-bisectors (first (:points v1)) (list)))) => 6)
;; this logic only holds if there are no two bisectors that are parallel.
;; the voronoi contains the same amount of points as it was given.
(fact (count (:points v1)) => (count points2))

(dorun
  (for [p (:points v1)]
    ;; each point has as many bisectors as other points exist.
    (fact (count (:bisectors p)) => (- (count points2) 1))))
(dorun
  (for [p (:points v1)]
    ;; each bisector has one intersection with the other bisectors.
    (fact (count (:intersections p)) => (/ (* (count points2) (- (count points2) 1)) 2))))

(def rp (p/point 1 1))
(def rl1 (l/line (p/point 0 1) (p/point 1 2)))
(def rl2 (l/line (p/point 0 2) (p/point 1 2)))
(def rl3 (l/line (p/point 0 3) (p/point 1 3)))
(def rl4 (l/line (p/point 0 0.5) (p/point 1 0.5)))
(def ri (p/point 2 3))
(def c1 (v/count-intersections rp ri (list rl1 rl2 rl3 rl4)))
(def cuts (l/cuts (l/line rp ri) (list rl1 rl2 rl3 rl4)))
(fact c1 => 1)
(def r1 (v/relevant? rp ri (list rl1 rl2 rl3 rl4)))
(fact r1 => false)
(def c1 (v/count-intersections rp ri (list rl1 rl3 rl4)))
(fact c1 => 0)
(def r1 (v/relevant? rp ri (list rl1 rl3 rl4)))
(fact r1 => true)


(def points3 (list p3 p4 p5))
(def v2 (v/voronoi points3 0 0 20 20))
; test that there are no nullpointers
(fact (count (v/cell-corners (nth (:points v2) 0))) => 1)
(fact (count (v/cell-corners (nth (:points v2) 1))) => 1)
(fact (count (v/cell-corners (nth (:points v2) 2))) => 1)

(def cell1 (v/cell-corners (first (:points v1))))
(fact (count cell1) => 3)
(def connected (v/connect-cell points3))
(fact (count connected) => 3)
(fact (:p1 (nth connected 0)) => p3)
(fact (:p2 (nth connected 0)) => p4)
(fact (:p1 (nth connected 1)) => p4)
(fact (:p2 (nth connected 1)) => p5)
(fact (:p1 (nth connected 2)) => p5)
(fact (:p2 (nth connected 2)) => p3)


(def points4 (list (p/point 5 5)))
(def v3 (v/voronoi points4 0 0 10 10))
(dorun (println (c/out v3)))
(dorun (println (map #(str (c/out (:intersection %)) "\n") (:intersections (first (:points v3))))))
(dorun (println (map #(str (p/angle (p/point 5 5) (p/point 0 0) (:intersection %)) "\n") (:intersections (first (:points v3))))))
(dorun (println (map #(str (c/out %)) (v/cell-corners (first (:points v3))))))
