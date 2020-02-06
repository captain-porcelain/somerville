;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.rasterization.conrec-test
  (:require
    [somerville.maps.grid :as grid]
    [somerville.rasterization.conrec :as conrec]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(defn center-spike
  [g]
  (dorun
    (for [x (range (:width g))
          y (range (:height g))]
        (grid/update-cell g x y #(assoc % :z (min x y (- (:width g) (inc x)) (- (:height g) (inc y))))))))

(defn sample-grid
  [size]
  (let [g (grid/grid size size)
        tmp (center-spike g)]
    g))

(defn one-spike-grid
  []
  (let [width 5
        g (grid/grid width width)
        tmp (dorun
              (for [x (range (:width g))
                    y (range (:height g))]
                (grid/update-cell g x y #(assoc % :z 0))))
        tmp (grid/update-cell g 2 2 #(assoc % :z 5))]
    g))

(deftest triangulation
  (let [t (conrec/triangulation (sample-grid 5))
        t0 (nth t 0)]
    (is (= 16 (count t)))
    (is (= 0 (:x (:p1 t0))))
    (is (= 0 (:y (:p1 t0))))
    (is (= 0 (:z (:p1 t0))))
    (is (= 1 (:x (:p2 t0))))
    (is (= 0 (:y (:p2 t0))))
    (is (= 0 (:z (:p2 t0))))
    (is (= 1 (:x (:p3 t0))))
    (is (= 1 (:y (:p3 t0))))
    (is (= 1 (:z (:p3 t0))))
    (is (= 0 (:x (:p4 t0))))
    (is (= 1 (:y (:p4 t0))))
    (is (= 0 (:z (:p4 t0))))

    (is (= 0.5 (:x (:p0 t0))))
    (is (= 0.5 (:y (:p0 t0))))
    (is (= 1/4 (:z (:p0 t0))))))

(deftest relative-height
  (let [t (conrec/relative-heights (conrec/triangulation (sample-grid 5)) 1)
        t0 (nth t 0)]
    (is (=  0 (:x (:p1 t0))))
    (is (=  0 (:y (:p1 t0))))
    (is (= -1 (:z (:p1 t0))))
    (is (=  1 (:x (:p2 t0))))
    (is (=  0 (:y (:p2 t0))))
    (is (= -1 (:z (:p2 t0))))
    (is (=  1 (:x (:p3 t0))))
    (is (=  1 (:y (:p3 t0))))
    (is (=  0 (:z (:p3 t0))))
    (is (=  0 (:x (:p4 t0))))
    (is (=  1 (:y (:p4 t0))))
    (is (= -1 (:z (:p4 t0))))

    (is (=  0.5 (:x (:p0 t0))))
    (is (=  0.5 (:y (:p0 t0))))
    (is (= -3/4 (:z (:p0 t0))))))

(deftest case-indices
  (let [t (conrec/relative-heights (conrec/triangulation (sample-grid 5)) 1)
        t0 (conrec/triangle-case-indices (nth t 0))
        t6 (conrec/triangle-case-indices (nth t 6))
        t7 (conrec/triangle-case-indices (nth t 7))]
    (is (= [:below :below :below :on :below] t0))
    (is (= [:above :on :on :on :above] t6))
    (is (= [:below :on :below :below :on] t7))))

(deftest corner-cases
  (let [t (conrec/relative-heights (conrec/triangulation (sample-grid 5)) 1)
        [tc1 tc2 tc3 tc4] (conrec/triangle-corner-cases (nth t 6))]
    (is (= [:on :above :on] tc1))
    (is (= [:on :above :on] tc2))
    (is (= [:on :above :above] tc3))
    (is (= [:above :above :on] tc4))))

(deftest spike-test
  (let [t (conrec/relative-heights (conrec/triangulation (one-spike-grid)) 2)
        [t5c1 t5c2 t5c3 t5c4] (conrec/triangle-corner-cases (nth t 5))
        [t6c1 t6c2 t6c3 t6c4] (conrec/triangle-corner-cases (nth t 6))
        [t9c1 t9c2 t9c3 t9c4] (conrec/triangle-corner-cases (nth t 9))
        [t10c1 t10c2 t10c3 t10c4] (conrec/triangle-corner-cases (nth t 10))
        [c51 c52 c53 c54] (map conrec/triangle-case (conrec/triangle-corner-cases (nth t 5)))
        [c61 c62 c63 c64] (map conrec/triangle-case (conrec/triangle-corner-cases (nth t 6)))
        [c91 c92 c93 c94] (map conrec/triangle-case (conrec/triangle-corner-cases (nth t 9)))
        [c101 c102 c103 c104] (map conrec/triangle-case (conrec/triangle-corner-cases (nth t 10)))]
    (is (= [:below :below :below] t5c1))
    (is (= [:below :below :above] t5c2))
    (is (= [:above :below :below] t5c3))
    (is (= [:below :below :below] t5c4))

    (is (= [:below :below :below] t6c1))
    (is (= [:below :below :below] t6c2))
    (is (= [:below :below :above] t6c3))
    (is (= [:above :below :below] t6c4))

    (is (= [:below :below :above] t9c1))
    (is (= [:above :below :below] t9c2))
    (is (= [:below :below :below] t9c3))
    (is (= [:below :below :below] t9c4))

    (is (= [:above :below :below] t10c1))
    (is (= [:below :below :below] t10c2))
    (is (= [:below :below :below] t10c3))
    (is (= [:below :below :above] t10c4))

    (is (= :no-line c51))
    (is (= :l2->l3 c52))
    (is (= :l3->l1 c53))
    (is (= :no-line c54))

    (is (= :no-line c61))
    (is (= :no-line c62))
    (is (= :l2->l3 c63))
    (is (= :l3->l1 c64))

    (is (= :l2->l3 c91))
    (is (= :l3->l1 c92))
    (is (= :no-line c93))
    (is (= :no-line c94))

    (is (= :l3->l1 c101))
    (is (= :no-line c102))
    (is (= :no-line c103))
    (is (= :l2->l3 c104))))
