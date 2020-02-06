(ns somerville.geometry.delaunay-test
  (:require
    [taoensso.timbre :as log]
    [somerville.geometry.commons :as sgc]
    [somerville.geometry.delaunay :as d]
    [somerville.geometry.point :as p]
    [somerville.geometry.triangle :as triangle]
    [somerville.geometry.line :as l])
  (:use clojure.test))


(deftest maxima
  (let [p1 (p/point 1 1)
        p2 (p/point 3 7)
        p3 (p/point 4 5)
        p4 (p/point 10 9)
        p5 (p/point 2 11)
        p6 (p/point 5 1)
        points (list p3 p4 p5 p6)]
    (is (= 10 (d/max-val points :x)))
    (is (= 11 (d/max-val points :y)))))

(deftest bounds
  (let [bp1 (p/point 0   0)
        bp2 (p/point 0  24)
        bp3 (p/point 22  0)
        p3 (p/point 4 5)
        p4 (p/point 10 9)
        p5 (p/point 2 11)
        p6 (p/point 5 1)
        points (list p3 p4 p5 p6)
        bt (d/bounding-triangle points)]
    (is (= bp1 (:p1 (:t bt))))
    (is (= bp2 (:p2 (:t bt))))
    (is (= bp3 (:p3 (:t bt))))))

(deftest invalidation
  (let [p1 (p/point 0 0)
        p2 (p/point 2 0)
        p3 (p/point 0 2)
        pn1 (p/point 3 3)
        pn2 (p/point 1.75 1.75)
        pn3 (p/point 2 2)
        t (triangle/triangle p1 p2 p3)
        dt (d/delaunay-triangle t)]
  (is (not (d/invalidates? dt pn1)))
  (is (d/invalidates? dt pn2))
  (is (d/invalidates? dt pn3))))

;(deftest adding
  ;(let [p1 (p/point 6 1)
        ;p2 (p/point 1 1)
        ;bt (d/bounding-triangle (list (p/point 3 3)))
        ;d1 (d/add-point (list bt) p1)
        ;d2 (d/add-point d1 p2)]
    ;;(log/info (str "triangulation 2: " (clojure.string/join "\n" (map str (:points d2)))))
    ;;(log/info d1)
    ;))
