(ns somerville.geometry.polygon-test
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon]
    [java.awt.image BufferedImage])
  (:require
    [somerville.image :as image]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.polygon :as poly])
  (:use clojure.test))

;(deftest polygon-creation
  ;(let [p1 (p/point -1  1)
        ;p2 (p/point  1  1)
        ;p3 (p/point  1 -1)
        ;p4 (p/point -1 -1)
        ;points (list p1 p2 p3 p4)
        ;polygon (poly/from-points points)]
    ;(is (= 4 (count (:lines polygon))))
    ;(is (= p1 (:p1 (nth (:lines polygon) 0))))
    ;(is (= p2 (:p2 (nth (:lines polygon) 0))))
    ;(is (= p2 (:p1 (nth (:lines polygon) 1))))
    ;(is (= p3 (:p2 (nth (:lines polygon) 1))))
    ;(is (= p3 (:p1 (nth (:lines polygon) 2))))
    ;(is (= p4 (:p2 (nth (:lines polygon) 2))))
    ;(is (= p4 (:p1 (nth (:lines polygon) 3))))
    ;(is (= p1 (:p2 (nth (:lines polygon) 3))))))

;(deftest intersections-between-line-and-polygon
  ;(let [p1 (p/point -1  1)
        ;p2 (p/point  1  1)
        ;p3 (p/point  1 -1)
        ;p4 (p/point -1 -1)
        ;points (list p1 p2 p3 p4)
        ;polygon (poly/from-points points)]
    ;(testing "Horizonzal line"
      ;(let [line (l/line (p/point -2 0) (p/point 2 0))
            ;intersections (poly/intersect polygon line)]
        ;(is (=  2 (count intersections)))
        ;(is (=  1 (:x (nth intersections 0))))
        ;(is (=  0 (:y (nth intersections 0))))
        ;(is (= -1 (:x (nth intersections 1))))
        ;(is (=  0 (:y (nth intersections 1))))))
    ;(testing "Vertical line"
      ;(let [line (l/line (p/point 0 2) (p/point 0 -2))
            ;intersections (poly/intersect polygon line)]
        ;(is (=  2 (count intersections)))
        ;(is (=  0 (:x (nth intersections 0))))
        ;(is (=  1 (:y (nth intersections 0))))
        ;(is (=  0 (:x (nth intersections 1))))
        ;(is (= -1 (:y (nth intersections 1))))))
    ;(testing "Horizonzal line as segments"
      ;(let [line (l/line (p/point -2 0) (p/point 2 0))
            ;intersections (poly/intersect-segments polygon line)]
        ;(is (=  2 (count intersections)))
        ;(is (=  1 (:x (nth intersections 0))))
        ;(is (=  0 (:y (nth intersections 0))))
        ;(is (= -1 (:x (nth intersections 1))))
        ;(is (=  0 (:y (nth intersections 1))))))
    ;(testing "Vertical line as segments"
      ;(let [line (l/line (p/point 0.1 2) (p/point 0.1 -2))
            ;intersections (poly/intersect polygon line)]
        ;(is (= 2 (count intersections)))
        ;(is (commons/close-to  0.1 (:x (nth intersections 0))))
        ;(is (commons/close-to  1   (:y (nth intersections 0))))
        ;(is (commons/close-to  0.1 (:x (nth intersections 1))))
        ;(is (commons/close-to -1   (:y (nth intersections 1))))))))


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

;(deftest cutting-polygon-with-line
  ;(let [p1 (p/point -1.0  1.0)
        ;p2 (p/point  1.0  1.0)
        ;p3 (p/point  1.0 -1.0)
        ;p4 (p/point -1.0 -1.0)
        ;center (p/point 0.0 0.0)
        ;points (list p1 p2 p3 p4)
        ;polygon (poly/from-points points center)]
    ;(testing "Horizontal line with two intersections"
      ;(let [line (l/line (p/point -2 -0.1) (p/point 2 -0.1))
            ;i1 (p/point  1.0 -0.1)
            ;i2 (p/point -1.0 -0.1)
            ;cut-result (poly/cut polygon line)]
        ;(is (poly/visible? (nth (:lines polygon) 0) line center))
        ;(is (not (poly/visible? (nth (:lines polygon) 1) line center)))
        ;(is (not (poly/visible? (nth (:lines polygon) 2) line center)))
        ;(is (not (poly/visible? (nth (:lines polygon) 3) line center)))
        ;(is (= 4 (count (:lines cut-result))))
        ;(is (= p1 (:p1 (nth (:lines cut-result) 0))))
        ;(is (= p2 (:p2 (nth (:lines cut-result) 0))))
        ;(is (= p2 (:p1 (nth (:lines cut-result) 1))))
        ;(is (= i1 (:p2 (nth (:lines cut-result) 1))))
        ;(is (= i1 (:p1 (nth (:lines cut-result) 2))))
        ;(is (= i2 (:p2 (nth (:lines cut-result) 2))))
        ;(is (= i2 (:p1 (nth (:lines cut-result) 3))))
        ;(is (= p1 (:p2 (nth (:lines cut-result) 3))))))
    ;(testing "Vertical line with two intersections"
      ;(let [line (l/line (p/point 0.1 2.0) (p/point 0.1 -2.0))
            ;i1 (p/point 0.1  1.0)
            ;i2 (p/point 0.1 -1.0)
            ;cut-result (poly/cut polygon line)]
        ;(is (= 4 (count (:lines cut-result))))
        ;(is (= p1 (:p1 (nth (:lines cut-result) 0))))
        ;(is (commons/close-to 0 (p/distance i1 (:p2 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance i1 (:p1 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i2 (:p2 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i2 (:p1 (nth (:lines cut-result) 2)))))
        ;(is (= p4 (:p2 (nth (:lines cut-result) 2))))
        ;(is (= p4 (:p1 (nth (:lines cut-result) 3))))
        ;(is (= p1 (:p2 (nth (:lines cut-result) 3))))))
    ;(testing "Horizontal line with one intersection"
      ;(let [lp1 (p/point -0.25 -0.5)
            ;lp2 (p/point  1.5  -0.5)
            ;line (l/line lp1 lp2)
            ;i1 (p/point  1.0 -0.5)
            ;i2 (p/point -0.5 -1.0)
            ;cut-result (poly/cut polygon line)]
        ;(is (= 6 (count (:lines cut-result))))
        ;(is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 5)))))))
    ;(testing "Line with one intersection affecting only one polygon line"
      ;(let [lp1 (p/point 0.75 -0.25)
            ;lp2 (p/point 1.5   0.5)
            ;line (l/line lp1 lp2)
            ;i1 (p/point 1.0 0.0)
            ;i2 (p/point 1.0 -0.333)
            ;cut-result (poly/cut polygon line)]
        ;(is (= 7 (count (:lines cut-result))))
        ;(is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance p3  (:p2 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance p3  (:p1 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 6)))))
        ;(is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 6)))))))
    ;(testing "Horizontal line with no intersections"
      ;(let [lp1 (p/point -0.5 -0.75)
            ;lp2 (p/point 0.5 -0.75)
            ;line (l/line lp1 lp2)
            ;i1 (p/point  0.666 -1.0)
            ;i2 (p/point -0.666 -1.0)
            ;cut-result (poly/cut polygon line)]
        ;(is (= 8 (count (:lines cut-result))))
        ;(is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance p3  (:p2 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance p3  (:p1 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance lp2 (:p2 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance lp2 (:p1 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 6)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 6)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 7)))))
        ;(is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 7)))))))
    ;(testing "Line covering one corner with no intersections"
      ;(let [lp1 (p/point 0.5  0)
            ;lp2 (p/point 0   -0.5)
            ;line (l/line lp1 lp2)
            ;i1 (p/point  1.0  0)
            ;i2 (p/point  0   -1)
            ;cut-result (poly/cut polygon line)]
        ;(is (= 7 (count (:lines cut-result))))
        ;(is (commons/close-to 0 (p/distance p1  (:p1 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p2 (nth (:lines cut-result) 0)))))
        ;(is (commons/close-to 0 (p/distance p2  (:p1 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p2 (nth (:lines cut-result) 1)))))
        ;(is (commons/close-to 0 (p/distance i1  (:p1 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p2 (nth (:lines cut-result) 2)))))
        ;(is (commons/close-to 0 (p/distance lp1 (:p1 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance lp2 (:p2 (nth (:lines cut-result) 3)))))
        ;(is (commons/close-to 0 (p/distance lp2 (:p1 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p2 (nth (:lines cut-result) 4)))))
        ;(is (commons/close-to 0 (p/distance i2  (:p1 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p2 (nth (:lines cut-result) 5)))))
        ;(is (commons/close-to 0 (p/distance p4  (:p1 (nth (:lines cut-result) 6)))))
        ;(is (commons/close-to 0 (p/distance p1  (:p2 (nth (:lines cut-result) 6)))))))
    ;))

(dorun (println "=========================================================================="))

(defn debug-draw
  "Create an image of size width x height with transparency and paint it completely black."
  [^String filename ^Integer width ^Integer height circle points polygon lines]
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
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) (:lines polygon)))
        tmp (.setPaint graphics Color/green)
        tmp (dorun (map #(do
                           (.drawLine graphics (- (:x %) 5) (- (:y %) 5) (+ (:x %) 5) (+ (:y %) 5))
                           (.drawLine graphics (- (:x %) 5) (+ (:y %) 5) (+ (:x %) 5) (- (:y %) 5))) points))
        tmp (.dispose graphics)]
    (image/write-image filename img)))

(def center (p/point 500 500))
(def circle (c/circle center 100))
(def points (c/circle-points circle 16))
(def polygon (poly/from-points points center))
(def line-1 (l/line (p/point 610 640) (p/point 105 140)))
(def line-2 (l/line (p/point 100 300) (p/point 600 300)))
(def line-3 (l/line (p/point 747 451) (p/point  61 505)))
(def lines (list line-1 line-2 line-3))
(debug-draw "/tmp/cut-0.png" 800 800 circle points polygon lines)
(def cut-1 (poly/cut polygon line-1))
(debug-draw "/tmp/cut-1.png" 800 800 circle points cut-1 lines)
(def cut-2 (poly/cut cut-1 line-2))
(debug-draw "/tmp/cut-2.png" 800 800 circle points cut-2 lines)
(def cut-3 (poly/cut cut-2 line-3))
(debug-draw "/tmp/cut-3.png" 800 800 circle points cut-3 lines)

(dorun (println "=========================================================================="))


