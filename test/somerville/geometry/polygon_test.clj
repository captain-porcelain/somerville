(ns somerville.geometry.polygon-test
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon]
    [java.awt.image BufferedImage])
  (:require
    [taoensso.timbre :as log]
    [taoensso.timbre.appenders.core :as appenders]
    [somerville.image :as image]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.polygon :as poly]
    [somerville.dungeons.discovery :as discovery])
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
    (testing "Line cuttin polygon once"
      (let [lp1 (p/point -0.5 0)
            lp2 (p/point  1.5 0)
            line (l/line lp1 lp2)
            cut (poly/shorten-line polygon line)]
        (is (= lp1 (:p1 cut)))
        (is (= (p/point 1.0 0.0) (:p2 cut)))))
    (testing "Line cuttin polygon twice"
      (let [lp1 (p/point -1.5 0)
            lp2 (p/point  1.5 0)
            line (l/line lp1 lp2)
            cut (poly/shorten-line polygon line)]
        (is (= (p/point -1.0 0.0) (:p1 cut)))
        (is (= (p/point  1.0 0.0) (:p2 cut)))))))

;(deftest updating-intersections-between-line-and-polygon
  ;(let [p1 (p/point -1.0  1.0)
        ;p2 (p/point  1.0  1.0)
        ;p3 (p/point  1.0 -1.0)
        ;p4 (p/point -1.0 -1.0)
        ;center (p/point 0.0 0.0)
        ;points (list p1 p2 p3 p4)
        ;polygon (poly/from-points points center)]
    ;(testing "Horizonzal line with two intersections"
      ;(let [line (l/line (p/point -2 -0.1) (p/point 2 -0.1))
            ;i1 (p/point  1.0 -0.1)
            ;i2 (p/point -1.0 -0.1)
            ;intersections (poly/find-intersections polygon line)
            ;updated-intersections (poly/update-intersections polygon line intersections)]
        ;(is (= 4 (count updated-intersections)))
        ;(is (= 0 (count (:intersections (nth updated-intersections 0)))))
        ;(is (= 1 (count (:intersections (nth updated-intersections 1)))))
        ;(is (= i1 (first (:intersections (nth updated-intersections 1)))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 2)))))
        ;(is (= 1 (count (:intersections (nth updated-intersections 3)))))
        ;(is (= i2 (first (:intersections (nth updated-intersections 3)))))))
    ;(testing "Vertical line with two intersections"
      ;(let [line (l/line (p/point 0.1 2.0) (p/point 0.1 -2.0))
            ;i1 (p/point 0.1  1.0)
            ;i2 (p/point 0.1 -1.0)
            ;intersections (poly/find-intersections polygon line)
            ;updated-intersections (poly/update-intersections polygon line intersections)]
        ;(is (= 4 (count updated-intersections)))
        ;(is (= 1 (count (:intersections (nth updated-intersections 0)))))
        ;(is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 0))))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 1)))))
        ;(is (= 1 (count (:intersections (nth updated-intersections 2)))))
        ;(is (commons/close-to 0 (p/distance i2 (first (:intersections (nth updated-intersections 2))))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 3)))))))
    ;(testing "Horizonzal line with one intersection"
      ;(let [line (l/line (p/point 0 -0.1) (p/point 2 -0.1))
            ;i1 (p/point 1.0 -0.1)
            ;i2 (p/point 0.0 -1.0)
            ;intersections (poly/find-intersections polygon line)
            ;updated-intersections (poly/update-intersections polygon line intersections)]
        ;(is (= 4 (count updated-intersections)))
        ;(is (= 0 (count (:intersections (nth updated-intersections 0)))))
        ;(is (= 1 (count (:intersections (nth updated-intersections 1)))))
        ;(is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 1))))))
        ;(is (= 1 (count (:intersections (nth updated-intersections 2)))))
        ;(is (commons/close-to 0 (p/distance i2 (first (:intersections (nth updated-intersections 2))))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 3)))))))
    ;(testing "Vertical line with one intersection"
      ;(let [line (l/line (p/point -0.1 2) (p/point -0.1 -2))
            ;i1 (p/point -0.1  1.0)
            ;i2 (p/point -0.1 -1.0)
            ;intersections (poly/find-intersections polygon line)
            ;updated-intersections (poly/update-intersections polygon line intersections)]
        ;(is (= 4 (count updated-intersections)))
        ;(is (= 1 (count (:intersections (nth updated-intersections 0)))))
        ;(is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 0))))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 1)))))
        ;(is (= 1 (count (:intersections (nth updated-intersections 2)))))
        ;(is (commons/close-to 0 (p/distance i2 (first (:intersections (nth updated-intersections 2))))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 3)))))))
    ;(testing "Horizonzal line with no intersections"
      ;(let [line (l/line (p/point -0.5 -0.75) (p/point 0.5 -0.75))
            ;i1 (p/point  0.666 -1.0)
            ;i2 (p/point -0.666 -1.0)
            ;intersections (poly/find-intersections polygon line)
            ;updated-intersections (poly/update-intersections polygon line intersections)]
        ;(is (= 4 (count updated-intersections)))
        ;(is (= 0 (count (:intersections (nth updated-intersections 0)))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 1)))))
        ;(is (= 2 (count (:intersections (nth updated-intersections 2)))))
        ;(is (commons/close-to 0 (p/distance i1 (first (:intersections (nth updated-intersections 2))))))
        ;(is (commons/close-to 0 (p/distance i2 (second (:intersections (nth updated-intersections 2))))))
        ;(is (= 0 (count (:intersections (nth updated-intersections 3)))))))))


(deftest shadow-intersections-between-line-and-polygon
  (log/info "================================================================================")
  (log/info "shadow-intersections-between-line-and-polygon")
  (let [p1 (p/point -1.0  1.0)
        p2 (p/point  1.0  1.0)
        p3 (p/point  1.0 -1.0)
        p4 (p/point -1.0 -1.0)
        center (p/point 0.0 0.0)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points center)]
    (testing "Horizonzal line with two intersections"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Horizonzal line with two intersections")
      (let [line (l/line (p/point -2 -0.1) (p/point 2 -0.1))
            i1 (p/point  1.0 -0.1)
            i2 (p/point -1.0 -0.1)
            intersections (poly/shadow-intersections polygon line)]
        (is (= 4 (count intersections)))
        (is (= 0 (count (:intersections (nth intersections 0)))))
        (is (= 1 (count (:intersections (nth intersections 1)))))
        (is (= i1 (first (:intersections (nth intersections 1)))))
        (is (= 0 (count (:intersections (nth intersections 2)))))
        (is (= 1 (count (:intersections (nth intersections 3)))))
        (is (= i2 (first (:intersections (nth intersections 3)))))))
    (testing "Vertical line with two intersections"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Vertical line with two intersections")
      (let [line (l/line (p/point 0.1 2.0) (p/point 0.1 -2.0))
            i1 (p/point 0.1  1.0)
            i2 (p/point 0.1 -1.0)
            intersections (poly/shadow-intersections polygon line)]
        (is (= 4 (count intersections)))
        (is (= 1 (count (:intersections (nth intersections 0)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth intersections 0))))))
        (is (= 0 (count (:intersections (nth intersections 1)))))
        (is (= 1 (count (:intersections (nth intersections 2)))))
        (is (commons/close-to 0 (p/distance i2 (first (:intersections (nth intersections 2))))))
        (is (= 0 (count (:intersections (nth intersections 3)))))))
    (testing "Horizonzal line with one intersection"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Horizonzal line with one intersection")
      (let [line (l/line (p/point 0 -0.1) (p/point 2 -0.1))
            i1 (p/point 1.0 -0.1)
            i2 (p/point 0.0 -1.0)
            intersections (poly/shadow-intersections polygon line)]
        (is (= 4 (count intersections)))
        (is (= 0 (count (:intersections (nth intersections 0)))))
        (is (= 1 (count (:intersections (nth intersections 1)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth intersections 1))))))
        (is (= 1 (count (:intersections (nth intersections 2)))))
        (is (commons/close-to 0 (p/distance i2 (first (:intersections (nth intersections 2))))))
        (is (= 0 (count (:intersections (nth intersections 3)))))))
    (testing "Vertical line with one intersection"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Vertical line with one intersection")
      (let [line (l/line (p/point -0.1 2) (p/point -0.1 -2))
            i1 (p/point -0.1  1.0)
            i2 (p/point -0.1 -1.0)
            intersections (poly/shadow-intersections polygon line)]
        (is (= 4 (count intersections)))
        (is (= 1 (count (:intersections (nth intersections 0)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth intersections 0))))))
        (is (= 0 (count (:intersections (nth intersections 1)))))
        (is (= 1 (count (:intersections (nth intersections 2)))))
        (is (commons/close-to 0 (p/distance i2 (first (:intersections (nth intersections 2))))))
        (is (= 0 (count (:intersections (nth intersections 3)))))))
    (testing "Line with one intersection with two polygon lines"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Line with one intersection with two polygon lines")
      (let [line (l/line (p/point 0.75 0.25) (p/point 1.0 1.0))
            i1 (p/point 1.0 0.333)
            i2 (p/point 1.0 1.0)
            intersections (poly/shadow-intersections polygon line)]
        (is (= 4 (count intersections)))
        (is (= 0 (count (:intersections (nth intersections 0)))))
        (is (= 2 (count (:intersections (nth intersections 1)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth intersections 1))))))
        (is (commons/close-to 0 (p/distance i2 (second (:intersections (nth intersections 1))))))
        (is (= 0 (count (:intersections (nth intersections 2)))))
        (is (= 0 (count (:intersections (nth intersections 3)))))))
    (testing "Horizonzal line with no intersections"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Horizonzal line with no intersections")
      (let [line (l/line (p/point -0.5 -0.75) (p/point 0.5 -0.75))
            i1 (p/point  0.666 -1.0)
            i2 (p/point -0.666 -1.0)
            intersections (poly/shadow-intersections polygon line)]
        (is (= 4 (count intersections)))
        (is (= 0 (count (:intersections (nth intersections 0)))))
        (is (= 0 (count (:intersections (nth intersections 1)))))
        (is (= 2 (count (:intersections (nth intersections 2)))))
        (is (commons/close-to 0 (p/distance i1 (first (:intersections (nth intersections 2))))))
        (is (commons/close-to 0 (p/distance i2 (second (:intersections (nth intersections 2))))))
        (is (= 0 (count (:intersections (nth intersections 3)))))))))

(deftest cutting-polygon-with-line
  (log/info "================================================================================")
  (log/info "cutting-polygon-with-line")
  (let [p1 (p/point -1.0  1.0)
        p2 (p/point  1.0  1.0)
        p3 (p/point  1.0 -1.0)
        p4 (p/point -1.0 -1.0)
        center (p/point 0.0 0.0)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points center)]
    (testing "Horizontal line with two intersections"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Horizontal line with two intersections")
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
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Vertical line with two intersections")
      (let [line (l/line (p/point 0.1 2.0) (p/point 0.1 -2.0))
            i1 (p/point 0.1  1.0)
            i2 (p/point 0.1 -1.0)
            cut-result (poly/cut polygon line)]
        (is (= 4 (count (:lines cut-result))))
        (is (commons/close-to 0 (p/distance p4 (:p1 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p1 (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p1 (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1 (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1 (:p1 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance i2 (:p2 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance i2 (:p1 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance p4 (:p2 (nth (:lines cut-result) 3)))))))
    (testing "Horizontal line with one intersection"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Horizontal line with one intersection")
      (let [lp1 (p/point -0.25 -0.5)
            lp2 (p/point  1.5  -0.5)
            line (l/line lp1 lp2)
            i1 (p/point  1.0 -0.5)
            i2 (p/point -0.5 -1.0)
            cut-result (poly/cut polygon line)]
        (is (= 6 (count (:lines cut-result))))
        (is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 5)))))))
    (testing "Line with one intersection affecting only one polygon line"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Line with one intersection affecting only one polygon line")
      (let [lp1 (p/point 0.75 -0.25)
            lp2 (p/point 1.5   0.5)
            line (l/line lp1 lp2)
            i1 (p/point 1.0 0.0)
            i2 (p/point 1.0 -0.333)
            cut-result (poly/cut polygon line)]
        (is (= 7 (count (:lines cut-result))))
        (is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance p3  (:p2 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance p3  (:p1 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 5)))))
        (is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 6)))))
        (is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 6)))))))
    (testing "Horizontal line with no intersections"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Horizontal line with no intersections")
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
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Line covering one corner with no intersections")
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
        (is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 6)))))))
    (testing "Line covering one corner of start and end of polygon"
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Line covering one corner of start and end of polygon")
      (let [lp1 (p/point -1.5 -0.5)
            lp2 (p/point  0.5  1.5)
            line (l/line lp1 lp2)
            i1 (p/point -1 0)
            i2 (p/point  0 1)
            cut-result (poly/cut polygon line)]
        (is (= 5 (count (:lines cut-result))))
        (is (commons/close-to 0 (p/distance p2 (:p1 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p3 (:p2 (nth (:lines cut-result) 0)))))
        (is (commons/close-to 0 (p/distance p3 (:p1 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance p4 (:p2 (nth (:lines cut-result) 1)))))
        (is (commons/close-to 0 (p/distance p4 (:p1 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance i1 (:p2 (nth (:lines cut-result) 2)))))
        (is (commons/close-to 0 (p/distance i1 (:p1 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance i2 (:p2 (nth (:lines cut-result) 3)))))
        (is (commons/close-to 0 (p/distance i2 (:p1 (nth (:lines cut-result) 4)))))
        (is (commons/close-to 0 (p/distance p2 (:p2 (nth (:lines cut-result) 4)))))))
    (testing "Casting shadow results in many intersections with polygon so special checking if point is on polygon line is needed.
             Also creates duplicate intersection."
      (log/info "--------------------------------------------------------------------------------")
      (log/info "Casting shadow results in many intersections with polygon so special checking if point is on polygon line is needed.
             Also creates duplicate intersection.")
      (let [l1p1 (p/point 0.5 0.25)
            l1p2 (p/point 1.5 0.25)
            line-1 (l/line l1p1 l1p2)
            i1 (p/point 1 0.5)
            i2 (p/point 1 0.25)
            cut-result-1 (poly/cut polygon line-1)
            l2p1 (p/point -0.5 0.25)
            l2p2 (p/point  0.5 0.25)
            line-2 (l/line l2p1 l2p2)
            i3 (p/point -1 0.5)
            cut-result-2 (poly/cut cut-result-1 line-2)
            ]
        (is (= 7 (count (:lines cut-result-1))))
        (is (commons/close-to 0 (p/distance p1   (:p1 (nth (:lines cut-result-1) 0)))))
        (is (commons/close-to 0 (p/distance p2   (:p2 (nth (:lines cut-result-1) 0)))))
        (is (commons/close-to 0 (p/distance p2   (:p1 (nth (:lines cut-result-1) 1)))))
        (is (commons/close-to 0 (p/distance i1   (:p2 (nth (:lines cut-result-1) 1)))))
        (is (commons/close-to 0 (p/distance i1   (:p1 (nth (:lines cut-result-1) 2)))))
        (is (commons/close-to 0 (p/distance l1p1 (:p2 (nth (:lines cut-result-1) 2)))))
        (is (commons/close-to 0 (p/distance l1p1 (:p1 (nth (:lines cut-result-1) 3)))))
        (is (commons/close-to 0 (p/distance i2   (:p2 (nth (:lines cut-result-1) 3)))))
        (is (commons/close-to 0 (p/distance i2   (:p1 (nth (:lines cut-result-1) 4)))))
        (is (commons/close-to 0 (p/distance p3   (:p2 (nth (:lines cut-result-1) 4)))))
        (is (commons/close-to 0 (p/distance p3   (:p1 (nth (:lines cut-result-1) 5)))))
        (is (commons/close-to 0 (p/distance p4   (:p2 (nth (:lines cut-result-1) 5)))))
        (is (commons/close-to 0 (p/distance p4   (:p1 (nth (:lines cut-result-1) 6)))))
        (is (commons/close-to 0 (p/distance p1   (:p2 (nth (:lines cut-result-1) 6)))))
        (is (= 6 (count (:lines cut-result-2))))
        (is (commons/close-to 0 (p/distance i2   (:p1 (nth (:lines cut-result-2) 0)))))
        (is (commons/close-to 0 (p/distance p3   (:p2 (nth (:lines cut-result-2) 0)))))
        (is (commons/close-to 0 (p/distance p3   (:p1 (nth (:lines cut-result-2) 1)))))
        (is (commons/close-to 0 (p/distance p4   (:p2 (nth (:lines cut-result-2) 1)))))
        (is (commons/close-to 0 (p/distance p4   (:p1 (nth (:lines cut-result-2) 2)))))
        (is (commons/close-to 0 (p/distance i3   (:p2 (nth (:lines cut-result-2) 2)))))
        (is (commons/close-to 0 (p/distance i3   (:p1 (nth (:lines cut-result-2) 3)))))
        (is (commons/close-to 0 (p/distance l2p1 (:p2 (nth (:lines cut-result-2) 3)))))
        (is (commons/close-to 0 (p/distance l2p1 (:p1 (nth (:lines cut-result-2) 4)))))
        (is (commons/close-to 0 (p/distance l2p2 (:p2 (nth (:lines cut-result-2) 4)))))
        (is (commons/close-to 0 (p/distance l2p2 (:p1 (nth (:lines cut-result-2) 5)))))
        (is (commons/close-to 0 (p/distance i2   (:p2 (nth (:lines cut-result-2) 5)))))))
    ))



(defn debug-draw
  "Create an image of size width x height with transparency and paint it completely black."
  [^String filename ^Integer width ^Integer height circle points polygon lines current]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/white)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.setPaint graphics Color/black)
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) lines))
        tmp (.setPaint graphics Color/red)
        tmp (.drawOval graphics (- (:x (:p circle)) (:r circle)) (- (:y (:p circle)) (:r circle)) (* 2 (:r circle)) (* 2 (:r circle)))
        tmp (.drawLine graphics (- (:x (:p circle)) 5) (- (:y (:p circle)) 5) (+ (:x (:p circle)) 5) (+ (:y (:p circle)) 5))
        tmp (.drawLine graphics (- (:x (:p circle)) 5) (+ (:y (:p circle)) 5) (+ (:x (:p circle)) 5) (- (:y (:p circle)) 5))
        tmp (.setPaint graphics Color/blue)
        tmp (dorun (map #(.drawLine graphics (get (:p1 %) :x 0) (get (:p1 %) :y 0) (get (:p2 %) :x 0) (get (:p2 %) :y 0)) (:lines polygon)))
        tmp (.setPaint graphics Color/green)
        tmp (dorun (map #(do
                           (.drawLine graphics (- (:x %) 5) (- (:y %) 5) (+ (:x %) 5) (+ (:y %) 5))
                           (.drawLine graphics (- (:x %) 5) (+ (:y %) 5) (+ (:x %) 5) (- (:y %) 5))) points))
        tmp (.setPaint graphics Color/yellow)
        tmp (when-not (nil? current) (.drawLine graphics (:x (:p1 current)) (:y (:p1 current)) (:x (:p2 current)) (:y (:p2 current))))
        is (if (nil? current) (list) (reduce concat (map :intersections (poly/update-intersections polygon current (poly/find-intersections polygon current)))))
        tmp (.setPaint graphics Color/orange)
        tmp (dorun (map #(do
                           (.drawLine graphics (- (:x %) 4) (- (:y %) 4) (+ (:x %) 4) (+ (:y %) 4))
                           (.drawLine graphics (- (:x %) 4) (+ (:y %) 4) (+ (:x %) 4) (- (:y %) 4))) is))
        tmp (.setPaint graphics Color/green)
        tmp (.drawLine graphics 186 1385 239 1385)
        tmp (.drawLine graphics 300 1305 239 1385)
        ;tmp (.drawLine graphics (:x (:p circle)) (:y (:p circle)) 374 1303)
        ;tmp (.drawLine graphics (:x (:p circle)) (:y (:p circle)) 392 1288)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

(defn debug-cut
  [circle lines width height]
  (let [points (c/circle-points circle 16)
        polygon (poly/from-points points (:p circle))]
    (loop [ls lines
           i 0
           cut polygon
           l nil]
      (let [tmp (log/info "--------------------------------------------------------------------------------")
            tmp (log/info "Debug Cut")
            tmp (log/info (str "iteration " i))
            tmp (try
                  (debug-draw (str "/tmp/cut-" i ".png") width height circle points cut lines l)
                  (catch Exception e (log/error e)))]
        (if (= 0 (count ls))
          i
          (let [new-cut (try (poly/cut cut (first ls))
                             (catch Exception e (do (println (str "Error cutting polygon on iteration " i))
                                                    (log/error e))))
                rest-ls (if (nil? new-cut) (list) (rest ls))]
            (recur rest-ls (inc i) new-cut (first ls))))))))

(defn debug-baramzigli
  []
  (log/info "================================================================================")
  (log/info "debug baramzigli")
  (let [circle (c/circle (p/point 161 1472) 300)
        lines (discovery/parse "line 1059,1402 1302,1372
                                line 552,1297 749,1404
                                line 374,1303 552,1297
                                line 239,1376 374,1303
                                line 881,1350 881,1408
                                line 879,1223 881,1294
                                line 1242,882 1239,570
                                line 1161,879 1242,882
                                line 1158,1036 1161,879
                                line 926,1036 1158,1036
                                line 932,1224 926,1036
                                line 1062,1220 802,1224
                                line 1061,1405 1062,1220
                                line 747,1401 1061,1405
                                line 741,1034 747,1401
                                line 241,1033 741,1034
                                line 239,1375 241,1033
                                line 186,1375 239,1375
                                line 717,934 716,1036
                                line 842,938 845,792
                                line 686,941 842,938
                                line 463,800 463,943
                                line 67,548 76,882
                                line 1018,566 1240,573
                                line 1034,492 1018,566
                                line 955,344 1034,492
                                line 871,243 955,344
                                line 655,168 871,243
                                line 439,231 655,168
                                line 325,395 439,231
                                line 288,560 325,395
                                line 56,555 288,560
                                line 0,1373 101,1373
                                line 40,940 40,1373
                                line 40,940 623,940
                                line 168,940 168,868
                                line 168,868 75,868")]
    (try
      (debug-cut circle lines 2000 2000)
      (catch Exception e (.printStackTrace e)))))


;(debug-baramzigli)
