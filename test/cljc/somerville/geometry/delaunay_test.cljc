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
  (let [bp1 (p/point 1   0)
        bp2 (p/point 1  24)
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

(deftest negative-bounds
  (let [bp1 (p/point 1  -2)
        bp2 (p/point 1  24)
        bp3 (p/point 22 -2)
        p3 (p/point  4  5)
        p4 (p/point 10  9)
        p5 (p/point  2 11)
        p6 (p/point  5 -1)
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

(deftest missing-line
  (let [width 1200
        height 800
        p0 (p/point -296,4.4666595458984375)
        p1 (p/point -360,31.466659545898438)
        p2 (p/point -395,69.46665954589844)
        p3 (p/point -430,38.46665954589844)
        p4 (p/point -417,-50.53334045410156)
        p5 (p/point -382,-144.53334045410156)
        p6 (p/point -141,2.4666595458984375)
        p7 (p/point -301,278.46665954589844)
        p8 (p/point -296,60.46665954589844)
        p9 (p/point -295,-232.53334045410156)
        points (list p0 p1 p2 p3 p4 p5 p6 p7 p8 p9)
        bound1 (p/point (* -1 width) (* -1 height))
        bound2 (p/point width height)
        d (d/delaunay points bound1 bound2)
        v (d/voronoi d)]
    (is (= 21 (count v)))))
