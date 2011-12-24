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
               intersections (intersect-two-parabolas p1 p2)]
           (is (= 2 (count intersections)))
           (is (= -0.5 (:x (first intersections))))
           (is (= 0.5 (:y (first intersections))))
           (is (= 0.5 (:x (second intersections))))
           (is (= 0.5 (:y (second intersections))))))

(deftest test-parabola-intersections-1
         (let [p1 (parabola-from-factors 1 0 0)
               p2 (parabola-from-factors 2 0 0)
               intersections (intersect-two-parabolas p1 p2)]
           (is (= 1 (count intersections)))
           (is (= 0 (:x (first intersections))))
           (is (= 0 (:y (first intersections))))))

(deftest test-parabola-intersections-1-2
         (let [point1 (struct-map point2 :x 0 :y 1)
               point2 (struct-map point2 :x 1 :y 1)
               directrix (struct-map line :a 0 :b -1)
               p1 (parabola-from-focuspoint-and-directrix point1 directrix)
               p2 (parabola-from-focuspoint-and-directrix point2 directrix)
               intersections (intersect-two-parabolas p1 p2)]
           (is (= 1 (count intersections)))
           (is (= 1/2 (:x (first intersections))))
           (is (= 1/16 (:y (first intersections))))))

(deftest test-parabola-intersections-0
         (let [p1 (parabola-from-factors 1 0 0)
               p2 (parabola-from-factors 1 0 1)
               intersections (intersect-two-parabolas p1 p2)]
           (is (= 0 (count intersections)))))

(deftest test-beachline-0
         (let [p1 (parabola-from-factors 1 0 0)
               p2 (parabola-from-factors 1 0 1)
               [intersections parabolas] (beachline (list p1 p2))]
           (is (= 0 (count intersections)))
           (is (= 1 (count parabolas)))
           (is (= p1 (first parabolas)))))

(deftest test-beachline-1
         (let [point1 (struct-map point2 :x 0 :y 1)
               point2 (struct-map point2 :x 1 :y 1)
               directrix (struct-map line :a 0 :b -1)
               p1 (parabola-from-focuspoint-and-directrix point1 directrix)
               p2 (parabola-from-focuspoint-and-directrix point2 directrix)
               [intersections parabolas] (beachline (list p1 p2))]
           (is (= 1 (count intersections)))
           (is (= 1/2 (:x (first intersections))))
           (is (= 1/16 (:y (first intersections))))
           (is (= 2 (count parabolas)))
           (is (= p1 (first parabolas)))
           (is (= p2 (second parabolas)))))

(deftest test-beachline-2
         (let [p1 (parabola-from-factors 2 0 0)
               p2 (parabola-from-factors -2 0 1)
               [intersections parabolas] (beachline (list p1 p2))]
           (is (= 2 (count intersections)))
           (is (= -0.5 (:x (first intersections))))
           (is (= 0.5 (:y (first intersections))))
           (is (= 0.5 (:x (second intersections))))
           (is (= 0.5 (:y (second intersections))))
           (is (= 3 (count parabolas)))
           (is (= p2 (nth parabolas 0)))
           (is (= p1 (nth parabolas 1)))
           (is (= p2 (nth parabolas 2)))
           (is (= p2 (get-parabola-from-beachline intersections parabolas -0.6)))
           (is (= p1 (get-parabola-from-beachline intersections parabolas -0.4)))
           (is (= p1 (get-parabola-from-beachline intersections parabolas 0.4)))
           (is (= p2 (get-parabola-from-beachline intersections parabolas 0.6)))
           ))

(deftest solve-beachline
         (let [point1 (struct-map point2 :x 500 :y 300)
               point2 (struct-map point2 :x 400 :y 400)
               directrix (struct-map line :a 0 :b 280)
               p1 (parabola-from-focuspoint-and-directrix point1 directrix)
               p2 (parabola-from-focuspoint-and-directrix point2 directrix)
               [intersections parabolas] (beachline (list p1 p2))]
           (is (= 2 (count intersections)))
           (is (= p2 (get-parabola-from-beachline intersections parabolas (- (:x (nth intersections 0)) 1))))
           (is (= p1 (get-parabola-from-beachline intersections parabolas (+ (:x (nth intersections 0)) 1))))
           (is (= p1 (get-parabola-from-beachline intersections parabolas (- (:x (nth intersections 1)) 1))))
           (is (= p2 (get-parabola-from-beachline intersections parabolas (+ (:x (nth intersections 1)) 1))))
           ))

(deftest solve-beachline-2
         (let [pnt1 (struct-map point2 :x -2 :y 2)
               pnt2 (struct-map point2 :x 0 :y 2)
               pnt3 (struct-map point2 :x 2 :y 2)
               directrix (struct-map line :a 0 :b 0)
               p1 (parabola-from-focuspoint-and-directrix pnt1 directrix)
               p2 (parabola-from-focuspoint-and-directrix pnt2 directrix)
               p3 (parabola-from-focuspoint-and-directrix pnt3 directrix)
               t (dorun (println p1))
               t (dorun (println p2))
               t (dorun (println p3))
               [intersections parabolas] (beachline (list p1 p2 p3))
               t (dorun (println (first intersections)))
               t (dorun (println (second intersections)))
               ]
           (is (= 2 (count intersections)))
           (is (= p1 (get-parabola-from-beachline intersections parabolas -2)))
           (is (= p2 (get-parabola-from-beachline intersections parabolas 0)))
           (is (= p3 (get-parabola-from-beachline intersections parabolas 2)))
           ))
