(ns somerville.geometry.polygon-test
  (:require
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.polygon :as poly])
  (:use clojure.test))

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
        (is (=  1 (:x (nth intersections 0))))
        (is (=  0 (:y (nth intersections 0))))
        (is (= -1 (:x (nth intersections 1))))
        (is (=  0 (:y (nth intersections 1))))))
    (testing "Vertical line as segments"
      (let [line (l/line (p/point 0.1 2) (p/point 0.1 -2))
            intersections (poly/intersect polygon line)]
        (is (= 2 (count intersections)))
        (is (commons/close-to  0.1 (:x (nth intersections 0))))
        (is (commons/close-to  1   (:y (nth intersections 0))))
        (is (commons/close-to  0.1 (:x (nth intersections 1))))
        (is (commons/close-to -1   (:y (nth intersections 1))))))))


(deftest updating-intersections-between-line-and-polygon
  (let [p1 (p/point -1.0  1.0)
        p2 (p/point  1.0  1.0)
        p3 (p/point  1.0 -1.0)
        p4 (p/point -1.0 -1.0)
        center (p/point 0.0 0.0)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points center)]
    (testing "Horizonzal line with two intersections"
      (let [line (l/line (p/point -2 -0.1) (p/point 2 -0.1))
            i1 (p/point  1.0 -0.1)
            i2 (p/point -1.0 -0.1)
            intersections (poly/find-intersections polygon line)
            updated-intersections (poly/update-intersections polygon line intersections)]
        (is (= 4 (count updated-intersections)))
        (is (= 0 (count (:intersections (nth updated-intersections 0)))))
        (is (= 1 (count (:intersections (nth updated-intersections 1)))))
        (is (= i1 (first (:intersections (nth updated-intersections 1)))))
        (is (= 0 (count (:intersections (nth updated-intersections 2)))))
        (is (= 1 (count (:intersections (nth updated-intersections 3)))))
        (is (= i2 (first (:intersections (nth updated-intersections 3)))))))
    (testing "Vertical line with two intersections"
      (let [line (l/line (p/point 0.1 2.0) (p/point 0.1 -2.0))
            i1 (p/point 0.1  1.0)
            i2 (p/point 0.1 -1.0)
            intersections (poly/find-intersections polygon line)
            updated-intersections (poly/update-intersections polygon line intersections)]
        (is (= 4 (count updated-intersections)))
        (is (= 1 (count (:intersections (nth updated-intersections 0)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 0))))))
        (is (= 0 (count (:intersections (nth updated-intersections 1)))))
        (is (= 1 (count (:intersections (nth updated-intersections 2)))))
        (is (commons/close-to 0 (p/distance i2 (first (:intersections (nth updated-intersections 2))))))
        (is (= 0 (count (:intersections (nth updated-intersections 3)))))))
    (testing "Horizonzal line with one intersection"
      (let [line (l/line (p/point 0 -0.1) (p/point 2 -0.1))
            i1 (p/point 1.0 -0.1)
            i2 (p/point 0.0 -1.0)
            intersections (poly/find-intersections polygon line)
            updated-intersections (poly/update-intersections polygon line intersections)]
        (is (= 4 (count updated-intersections)))
        (is (= 0 (count (:intersections (nth updated-intersections 0)))))
        (is (= 1 (count (:intersections (nth updated-intersections 1)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 1))))))
        (is (= 1 (count (:intersections (nth updated-intersections 2)))))
        (is (commons/close-to 0 (p/distance i2 (first (:intersections (nth updated-intersections 2))))))
        (is (= 0 (count (:intersections (nth updated-intersections 3)))))))
    (testing "Vertical line with one intersection"
      (let [line (l/line (p/point -0.1 2) (p/point -0.1 -2))
            i1 (p/point -0.1  1.0)
            i2 (p/point -0.1 -1.0)
            intersections (poly/find-intersections polygon line)
            updated-intersections (poly/update-intersections polygon line intersections)]
        (is (= 4 (count updated-intersections)))
        (is (= 1 (count (:intersections (nth updated-intersections 0)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 0))))))
        (is (= 0 (count (:intersections (nth updated-intersections 1)))))
        (is (= 1 (count (:intersections (nth updated-intersections 2)))))
        (is (commons/close-to 0 (p/distance i2 (first (:intersections (nth updated-intersections 2))))))
        (is (= 0 (count (:intersections (nth updated-intersections 3)))))))
    (testing "Horizonzal line with no intersections"
      (let [line (l/line (p/point -0.5 -0.75) (p/point 0.5 -0.75))
            i1 (p/point  0.666 -1.0)
            i2 (p/point -0.666 -1.0)
            intersections (poly/find-intersections polygon line)
            updated-intersections (poly/update-intersections polygon line intersections)]
        (is (= 4 (count updated-intersections)))
        (is (= 0 (count (:intersections (nth updated-intersections 0)))))
        (is (= 0 (count (:intersections (nth updated-intersections 1)))))
        (is (= 2 (count (:intersections (nth updated-intersections 2)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 2))))))
        (is (commons/close-to 0 (p/distance i2 (second (:intersections (nth updated-intersections 2))))))
        (is (= 0 (count (:intersections (nth updated-intersections 3)))))))))

(deftest cutting-polygon-with-line
  (let [p1 (p/point -1.0  1.0)
        p2 (p/point  1.0  1.0)
        p3 (p/point  1.0 -1.0)
        p4 (p/point -1.0 -1.0)
        center (p/point 0.0 0.0)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points center)]
    (testing "Horizontal line with two intersections"
      (let [line (l/line (p/point -2 -0.1) (p/point 2 -0.1))
            i1 (p/point  1.0 -0.1)
            i2 (p/point -1.0 -0.1)
            cut-result (poly/cut polygon line)]
        (is (poly/visible? (nth (:lines polygon) 0) line center))
        (is (not (poly/visible? (nth (:lines polygon) 1) line center)))
        (is (not (poly/visible? (nth (:lines polygon) 2) line center)))
        (is (not (poly/visible? (nth (:lines polygon) 3) line center)))
        (is (= 4 (count (:lines cut-result))))
        (is (= p1 (:p1 (nth (:lines cut-result) 0))))
        (is (= p2 (:p2 (nth (:lines cut-result) 0))))
        (is (= p2 (:p1 (nth (:lines cut-result) 1))))
        (is (= i1 (:p2 (nth (:lines cut-result) 1))))
        (is (= i1 (:p1 (nth (:lines cut-result) 2))))
        (is (= i2 (:p2 (nth (:lines cut-result) 2))))
        (is (= i2 (:p1 (nth (:lines cut-result) 3))))
        (is (= p1 (:p2 (nth (:lines cut-result) 3))))))
    (testing "Vertical line with two intersections"
      (let [line (l/line (p/point 0.1 2.0) (p/point 0.1 -2.0))
            i1 (p/point 0.1  1.0)
            i2 (p/point 0.1 -1.0)
            cut-result (poly/cut polygon line)]
        (is (= 4 (count (:lines cut-result))))
        (is (= p1 (:p1 (nth (:lines cut-result) 0))))
        (is (commons/close-to 0 (p/distance i1 (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance i1 (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i2 (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i2 (:p1 (nth (:lines cut-result) 2)))))
        (is (= p4 (:p2 (nth (:lines cut-result) 2))))
        (is (= p4 (:p1 (nth (:lines cut-result) 3))))
        (is (= p1 (:p2 (nth (:lines cut-result) 3))))))
    (testing "Horizontal line with no intersections"
      (let [lp1 (p/point -0.5 -0.75)
            lp2 (p/point 0.5 -0.75)
            line (l/line lp1 lp2)
            i1 (p/point  0.666 -1.0)
            i2 (p/point -0.666 -1.0)
            cut-result (poly/cut polygon line)]
        (is (= 8 (count (:lines cut-result))))
        (is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance p3  (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance p3  (:p1 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance lp2 (:p2 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance lp2 (:p1 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 6)))))
        (is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 6)))))
        (is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 7)))))
        (is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 7)))))))
    (testing "Line covering one corner with no intersections"
      (let [lp1 (p/point 0.5  0)
            lp2 (p/point 0   -0.5)
            line (l/line lp1 lp2)
            i1 (p/point  1.0  0)
            i2 (p/point  0   -1)
            cut-result (poly/cut polygon line)]
        (is (= 7 (count (:lines cut-result))))
        (is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance lp2 (:p2 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance lp2 (:p1 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 6)))))
        (is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 6)))))
        ))
    ))


