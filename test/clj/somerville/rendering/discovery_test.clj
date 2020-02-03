;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.maps.discovery-test
  (:require
    [somerville.rendering.discovery :as rcwt]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(def wall-description
  "line 10,10 30,10
   blubber
   line 10.1,30 10
   line 10.1,30
   line 30,10 30,30")

(deftest parsing
  (let [parsed (rcwt/parse wall-description)]
    (is (= 2 (count parsed)))
    (is (= (l/line (p/point 10 10) (p/point 30 10)) (nth parsed 0)))
    (is (= (l/line (p/point 30 10) (p/point 30 30)) (nth parsed 1)))))


(deftest relevant-walls
  (let [l1 (l/line (p/point -0.5 0) (p/point 0.5 0))
        l2 (l/line (p/point -0.5 0) (p/point 1.5 0))
        l3 (l/line (p/point -1.5 0) (p/point 1.5 0))
        l4 (l/line (p/point -1.5 2) (p/point 1.5 2))
        cp1 (p/point -1  0)
        cp2 (p/point  0 -1)
        cp3 (p/point  1  0)
        cp4 (p/point  0  1)
        walls (list l1 l2 l3 l4)
        relevant-walls (rcwt/relevant-walls (p/point 0 0) walls 1 4)]
    (is (= 7 (count relevant-walls)))
    ;(dorun (map #(println (commons/out %)) relevant-walls))
    (is (commons/close-to 0 (p/distance cp1 (:p1 (nth relevant-walls 0)))))
    (is (commons/close-to 0 (p/distance cp2 (:p2 (nth relevant-walls 0)))))
    (is (commons/close-to 0 (p/distance cp2 (:p1 (nth relevant-walls 1)))))
    (is (commons/close-to 0 (p/distance cp3 (:p2 (nth relevant-walls 1)))))
    (is (commons/close-to 0 (p/distance cp3 (:p1 (nth relevant-walls 2)))))
    (is (commons/close-to 0 (p/distance cp4 (:p2 (nth relevant-walls 2)))))
    (is (commons/close-to 0 (p/distance cp4 (:p1 (nth relevant-walls 3)))))
    (is (commons/close-to 0 (p/distance cp1 (:p2 (nth relevant-walls 3)))))
    (is (= l1 (nth relevant-walls 4)))
    (is (commons/close-to 0 (p/distance (p/point -0.5 0) (:p1 (nth relevant-walls 5)))))
    (is (commons/close-to 0 (p/distance (p/point  1   0) (:p2 (nth relevant-walls 5)))))
    (is (commons/close-to 0 (p/distance (p/point -1   0) (:p1 (nth relevant-walls 6)))))
    (is (commons/close-to 0 (p/distance (p/point  1   0) (:p2 (nth relevant-walls 6)))))))

(deftest cut-walls
  (let [l1 (l/line (p/point 2 1) (p/point 2 5))
        l2 (l/line (p/point 1 2) (p/point 3 2))
        l3 (l/line (p/point 1 3) (p/point 3 3))
        l4 (l/line (p/point 1 4) (p/point 2 4))
        lines (list l1 l2 l3 l4)
        cuts (rcwt/cut-walls lines)]
    (is (= 9 (count cuts)))
    (is (commons/close-to 0 (p/distance (p/point 2 1) (:p1 (nth cuts 0)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p2 (nth cuts 0)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p1 (nth cuts 1)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p2 (nth cuts 1)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p1 (nth cuts 2)))))
    (is (commons/close-to 0 (p/distance (p/point 2 4) (:p2 (nth cuts 2)))))
    (is (commons/close-to 0 (p/distance (p/point 2 4) (:p1 (nth cuts 3)))))
    (is (commons/close-to 0 (p/distance (p/point 2 5) (:p2 (nth cuts 3)))))
    (is (commons/close-to 0 (p/distance (p/point 1 2) (:p1 (nth cuts 4)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p2 (nth cuts 4)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p1 (nth cuts 5)))))
    (is (commons/close-to 0 (p/distance (p/point 3 2) (:p2 (nth cuts 5)))))
    (is (commons/close-to 0 (p/distance (p/point 1 3) (:p1 (nth cuts 6)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p2 (nth cuts 6)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p1 (nth cuts 7)))))
    (is (commons/close-to 0 (p/distance (p/point 3 3) (:p2 (nth cuts 7)))))
    (is (commons/close-to 0 (p/distance (p/point 1 4) (:p1 (nth cuts 8)))))
    (is (commons/close-to 0 (p/distance (p/point 2 4) (:p2 (nth cuts 8)))))))

(deftest point-sorting
  (testing "q1 simple"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point  1  1)
          p2 (p/point  2  1)
          l1 (l/line p1 p2)
          s1 (rcwt/sort-line-points l1 cp rp)]
      (is (= p1 (:p1 s1)))
      (is (= p2 (:p2 s1)))))
  (testing "q1 reverse"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point  2  1)
          p2 (p/point  1  1)
          l1 (l/line p1 p2)
          s1 (rcwt/sort-line-points l1 cp rp)]
      (is (= p2 (:p1 s1)))
      (is (= p1 (:p2 s1)))))
  (testing "q2"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point -2  1)
          p2 (p/point -1  1)
          l1 (l/line p1 p2)
          s1 (rcwt/sort-line-points l1 cp rp)]
      (is (= p1 (:p1 s1)))
      (is (= p2 (:p2 s1)))))
  (testing "multiple quadrants"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point -2 -2)
          p2 (p/point  1  1)
          l1 (l/line p1 p2)
          s1 (rcwt/sort-line-points l1 cp rp)]
      (is (= p2 (:p1 s1)))
      (is (= p1 (:p2 s1))))))

