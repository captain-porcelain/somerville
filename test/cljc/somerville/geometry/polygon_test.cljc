(ns somerville.geometry.polygon-test
  (:require
    [taoensso.timbre :as log]
    [taoensso.timbre.appenders.core :as appenders]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.polygon :as poly])
  (:use clojure.test))

(log/set-config!
  {:level :debug
   :appenders {:spit (appenders/spit-appender {:fname "/tmp/somerville-polygon-test.log"})}})

(deftest polygon-creation
  (let [p1 (p/point -1  1)
        p2 (p/point  1  1)
        p3 (p/point  1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points)]
    (is (= 4 (count (:lines polygon))))
    (is (= p1 (:p1 (nth (:lines polygon) 0))))
    (is (= p2 (:p2 (nth (:lines polygon) 0))))
    (is (= p2 (:p1 (nth (:lines polygon) 1))))
    (is (= p3 (:p2 (nth (:lines polygon) 1))))
    (is (= p3 (:p1 (nth (:lines polygon) 2))))
    (is (= p4 (:p2 (nth (:lines polygon) 2))))
    (is (= p4 (:p1 (nth (:lines polygon) 3))))
    (is (= p1 (:p2 (nth (:lines polygon) 3))))))

(deftest intersections-between-line-and-polygon
  (log/info "================================================================================")
  (log/info "intersections-between-line-and-polygon")
  (let [p1 (p/point -1  1)
        p2 (p/point  1  1)
        p3 (p/point  1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points)]
    (testing "Horizonzal line"
      (let [line (l/line (p/point -2 0) (p/point 2 0))
            intersections (poly/intersect polygon line)]
        (is (=  2 (count intersections)))
        (is (=  1 (:x (nth intersections 0))))
        (is (=  0 (:y (nth intersections 0))))
        (is (= -1 (:x (nth intersections 1))))
        (is (=  0 (:y (nth intersections 1))))))
    (testing "Vertical line"
      (let [line (l/line (p/point 0 2) (p/point 0 -2))
            intersections (poly/intersect polygon line)]
        (is (=  2 (count intersections)))
        (is (=  0 (:x (nth intersections 0))))
        (is (=  1 (:y (nth intersections 0))))
        (is (=  0 (:x (nth intersections 1))))
        (is (= -1 (:y (nth intersections 1))))))
    (testing "Horizonzal line as segments"
      (let [line (l/line (p/point -2 0) (p/point 2 0))
            intersections (poly/intersect-segments polygon line)]
        (is (=  2 (count intersections)))
        (is (= -1 (:x (nth intersections 0))))
        (is (=  0 (:y (nth intersections 0))))
        (is (=  1 (:x (nth intersections 1))))
        (is (=  0 (:y (nth intersections 1))))))
    (testing "Vertical line as segments"
      (let [line (l/line (p/point 0.1 2) (p/point 0.1 -2))
            intersections (poly/intersect polygon line)]
        (is (= 2 (count intersections)))
        (is (commons/close-to  0.1 (:x (nth intersections 0))))
        (is (commons/close-to  1   (:y (nth intersections 0))))
        (is (commons/close-to  0.1 (:x (nth intersections 1))))
        (is (commons/close-to -1   (:y (nth intersections 1))))))))

(deftest cutting-lines
  (let [p1 (p/point -1  1)
        p2 (p/point  1  1)
        p3 (p/point  1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points (p/point 0 0))]
    (testing "Line inside polygon"
      (let [lp1 (p/point -0.5 0)
            lp2 (p/point  0.5 0)
            line (l/line lp1 lp2)
            cut (poly/shorten-line polygon line)]
        (is (= lp1 (:p1 cut)))
        (is (= lp2 (:p2 cut)))))
    (testing "Line cutting polygon once"
      (let [lp1 (p/point -0.5 0)
            lp2 (p/point  1.5 0)
            line (l/line lp1 lp2)
            cut (poly/shorten-line polygon line)]
        (is (= lp1 (:p1 cut)))
        (is (= (p/point 1.0 0.0) (:p2 cut)))))
    (testing "Line cutting polygon twice"
      (let [lp1 (p/point -1.5 0)
            lp2 (p/point  1.5 0)
            line (l/line lp1 lp2)
            cut (poly/shorten-line polygon line)]
        (is (= (p/point -1.0 0.0) (:p1 cut)))
        (is (= (p/point  1.0 0.0) (:p2 cut)))))))

(deftest centroid-2d
  (let [p1 (p/point 1 0)
        p2 (p/point 1 1)
        p3 (p/point 0 1)
        p4 (p/point 0 0)
        polygon (poly/from-points (list p1 p2 p3 p4))]
    (is (= (p/point 1/2 1/2) (poly/centroid-2d polygon)))))

(deftest centroid-3d
  (let [p1 (p/point 1 0 2)
        p2 (p/point 1 1 2)
        p3 (p/point 0 1 2)
        p4 (p/point 0 0 2)
        polygon (poly/from-points (list p1 p2 p3 p4))]
    (is (= (p/point 1/2 1/2 2) (poly/centroid-3d polygon)))))


(deftest outline-graph
  (let [pa1 (p/point 2 3)
        pa2 (p/point 4 5)
        pa3 (p/point 6 1)
        pb1 (p/point 5 4)
        pb2 (p/point 7 4)
        pb3 (p/point 7 2)
        pb4 (p/point 5 2)
        p1 (poly/from-points (list pa1 pa2 pa3))
        p2 (poly/from-points (list pb1 pb2 pb3 pb4))
        o (poly/outline p1 p2)
        ops (poly/to-points o)
        pi1 (p/point 5 3)
        pi2 (p/point 11/2 2)]
    (is (= pa1 (nth ops 0)))
    (is (= pa2 (nth ops 1)))
    (is (= pi1 (nth ops 2)))
    (is (= pb1 (nth ops 3)))
    (is (= pb2 (nth ops 4)))
    (is (= pb3 (nth ops 5)))
    (is (= pi2 (nth ops 6)))
    (is (= pa3 (nth ops 7)))))

