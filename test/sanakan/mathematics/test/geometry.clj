(ns sanakan.mathematics.test.geometry
  (:use [sanakan.mathematics.geometry])
  (:use [clojure.test]))

(deftest test-simple-parabola
         (let [p (parabola-from-factors 1 2 3)]
           (is (= 1 (:a p)))
           (is (= 2 (:b p)))
           (is (= 3 (:c p)))))

(deftest test-parabola-from-focuspoint-and-directrix
         (let [point (struct-map point2 :x 0 :y 1)
               directrix (struct-map line :a 0 :b -1)
               p (parabola-from-focuspoint-and-directrix point directrix)]
           (is (= (/ 1 4) (:a p)))
           (is (= 0 (:b p)))
           (is (= 0 (:c p)))))

(deftest test-discriminate
         (let [p1 (parabola-from-factors 2 0 0)
               dis (discriminate p1)]
           (is (= 0 dis))))

(deftest test-discriminate
         (let [p1 (parabola-from-factors 2 0 1)
               dis (discriminate p1)]
           (is (= -0.5 dis))))

(deftest test-discriminate
         (let [p1 (parabola-from-factors 0 0 -1)
               dis (discriminate p1)]
           (is (= -1 dis))))

(deftest test-parabola-intersections-2
         (let [p1 (parabola-from-factors 2 0 0)
               p2 (parabola-from-factors -2 0 1)
               intersections (intersect p1 p2)]
           (is (= 2 (count intersections)))
           (is (= -0.5 (:x (first intersections))))
           (is (= 0.5 (:y (first intersections))))
           (is (= 0.5 (:x (second intersections))))
           (is (= 0.5 (:y (second intersections))))))

(deftest test-parabola-intersections-1
         (let [p1 (parabola-from-factors 1 0 0)
               p2 (parabola-from-factors 2 0 0)
               intersections (intersect p1 p2)]
           (is (= 1 (count intersections)))
           (is (= 0 (:x (first intersections))))
           (is (= 0 (:y (first intersections))))))

(deftest test-parabola-intersections-0
         (let [p1 (parabola-from-factors 1 0 0)
               p2 (parabola-from-factors 1 0 1)
               intersections (intersect p1 p2)]
           (is (= 0 (count intersections)))))

