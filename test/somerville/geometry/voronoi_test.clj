(ns somerville.geometry.voronoi-test
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.voronoi :as v]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

;; test bisector helper functions
(def p1 (p/point 1 1))
(def p2 (p/point 3 7))
(def points1 (list p1 p2))
(def bisectors1 (v/bisectors points1))
(deftest helpers
  (is (= (count bisectors1) 2))
  (is (= (count (:bisectors (first bisectors1))) 1))
  (is (= (:point (first bisectors1)) p1))
  (is (= (count (:bisectors (second bisectors1))) 1))
  (is (= (:point (second bisectors1)) p2)))

;; test properties of calculated voronois
(def p3 (p/point 4 5))
(def p4 (p/point 10 9))
(def p5 (p/point 2 10))
(def p6 (p/point 5 1))
(def points2 (list p3 p4 p5 p6))
(def v1 (v/voronoi points2 0 0 12 12))

(deftest properties-of-calculated-voronoi
  (is (= (count (:intersections (v/intersect-bisectors (first (:points v1)) (list)))) 6))
  ;; this logic only holds if there are no two bisectors that are parallel.
  ;; the voronoi contains the same amount of points as it was given.
  (is (= (count (:points v1)) (count points2)))

  (dorun
    (for [p (:points v1)]
      ;; each point has as many bisectors as other points exist.
      (is (= (count (:bisectors p)) (- (count points2) 1))))))

(def rp (p/point 1 1))
(def rl1 (l/line (p/point 0 1) (p/point 1 2)))
(def rl2 (l/line (p/point 0 2) (p/point 1 2)))
(def rl3 (l/line (p/point 0 3) (p/point 1 3)))
(def rl4 (l/line (p/point 0 0.5) (p/point 1 0.5)))
(def ri (p/point 2 3))
(def c1 (v/count-intersections rp ri (list rl1 rl2 rl3 rl4)))
(def cuts (l/cuts (l/line rp ri) (list rl1 rl2 rl3 rl4)))
(def r1 (v/relevant? rp ri (list rl1 rl2 rl3 rl4)))
(def c2 (v/count-intersections rp ri (list rl1 rl3 rl4)))
(def r2 (v/relevant? rp ri (list rl1 rl3 rl4)))
(deftest intersections
  (is (= c1 1))
  (is (= r1 false))
  (is (= c2 0))
  (is (= r2 true)))


(def points3 (list p3 p4 p5))
(def v2 (v/voronoi points3 0 0 20 20))
; test that there are no nullpointers
(deftest cells-without-nullpointers
  (is (= (count (v/cell-corners (nth (:points v2) 0))) 6))
  (is (= (count (v/cell-corners (nth (:points v2) 1))) 7))
  (is (= (count (v/cell-corners (nth (:points v2) 2))) 6)))

(def cell1 (v/cell-corners (first (:points v1))))
(def connected (v/connect-cell points3))
(deftest connected-cell
  (is (= (count cell1) 7))
  (is (= (count connected) 3))
  (is (= (:p1 (nth connected 0)) p3))
  (is (= (:p2 (nth connected 0)) p4))
  (is (= (:p1 (nth connected 1)) p4))
  (is (= (:p2 (nth connected 1)) p5))
  (is (= (:p1 (nth connected 2)) p5))
  (is (= (:p2 (nth connected 2)) p3)))

(defn rand-point
  []
  (p/point (rand-int 1000) (rand-int 1000)))

;(def points4 (list (p/point 5 5)))
;(def v3 (v/voronoi points4 0 0 10 10))
;(dorun (println (c/out v3)))
;(dorun (println (map #(str (c/out (:intersection %)) "\n") (:intersections (first (:points v3))))))
;(dorun (println (map #(str (p/angle (p/point 5 5) (p/point 0 0) (:intersection %)) "\n") (:intersections (first (:points v3))))))
;(dorun (println (map #(str (c/out (:intersection %)) "\n") (sort-by #(p/angle (p/point 5 5) (p/point 0 0) (:intersection %)) (:intersections (first (:points v3)))))))
;(dorun (println (map #(str (c/out %)) (v/cell-corners (first (:points v3))))))
