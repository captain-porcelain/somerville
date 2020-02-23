(ns somerville.geometry.point-test
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.commons :as c])
  (:use clojure.test))

(def p1 (p/point -1 -2))
(deftest basics
  (is (= (:x p1) -1))
  (is (= (:y p1) -2)))

(def p2 (p/point 3 5))
(def midp1p2 (p/midpoint p1 p2))
(deftest midpoints
  (is (= (:x midp1p2) 1))
  (is (= (:y midp1p2) 3/2)))

(deftest slopes
  (is (= (p/slope p1 p2) 7/4))
  (is (= (p/slope (p/point 0 0) (p/point 1 0)) 0))
  (is (= (p/slope (p/point 0 0) (p/point 1 1)) 1)))

(deftest find-quadrant
  (is (= (p/quadrant (p/point  0  0)) 1))
  (is (= (p/quadrant (p/point  1  0)) 1))
  (is (= (p/quadrant (p/point  1  1)) 1))
  (is (= (p/quadrant (p/point  0  1)) 2))
  (is (= (p/quadrant (p/point -1  1)) 2))
  (is (= (p/quadrant (p/point -1  0)) 3))
  (is (= (p/quadrant (p/point -1 -1)) 3))
  (is (= (p/quadrant (p/point  1 -1)) 4)))

(def PI00 0.0)
(def PI05 (/ Math/PI 2))
(def PI10 Math/PI)
(def PI15 (* 3 (/ Math/PI 2)))
(def PI20 (* 2 Math/PI))
(def PI175 (* 7 (/ Math/PI 4)))

(deftest angles
  (is (= (c/close-to (p/angle-to-x (p/point  1  0)) PI00)  true))
  (is (= (c/close-to (p/angle-to-x (p/point  0  1)) PI05) true))
  (is (= (c/close-to (p/angle-to-x (p/point -1  0)) PI10) true))
  (is (= (c/close-to (p/angle-to-x (p/point  0 -1)) PI15) true))
  (is (= (c/close-to (p/angle-to-x (p/point  5 -5)) PI175) true))

  (is (= (c/close-to (p/angle (p/point 0 0) (p/point 1 0) (p/point 1 0)) PI00) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point 1 0) (p/point 0 1)) PI05) true))

  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point  1  0)) PI00) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point  0  1)) PI05) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point -1  0)) PI10) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point  0 -1)) PI15) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point -1  0) (p/point  1  0)) (* -1 PI10)) true)))

  ;(is (= (c/close-to (p/angle (p/point 5 5) (p/point  0  0) (p/point 10  0)) PI05) true))

(deftest distances
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI00 1) (p/point  1.0  0.0)) 0) true))
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI05 1) (p/point  0.0  1.0)) 0) true))
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI10 1) (p/point -1.0  0.0)) 0) true))
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI15 1) (p/point  0.0 -1.0)) 0) true)))

(deftest linear-combination
  (let [v (p/point 2 2)
        s (p/point 1 0)
        t (p/point 0 1)
        [a b] (p/linear-combination v s t)]
    (is (= 2 a))
    (is (= 2 b))))

(deftest average
  (let [p1 (p/point 2 2)
        p2 (p/point 1 0)
        p3 (p/point 0 1)
        pa (p/average (list p1 p2 p3))]
    (is (= (p/point 1 1 0) pa))))

(deftest angles-3d
  (let [px1 (p/point 1 0 0)
        [alphax1 betax1] (p/p3d->angles px1)
        tmp (dorun (println (str alphax1 " , " betax1)))
        px1t (p/angles->p3d alphax1 betax1)

        pnx (p/point -1 0 0)
        [alphanx betanx] (p/p3d->angles pnx)
        tmp (dorun (println (str alphanx " , " betanx)))
        pnxt (p/angles->p3d alphanx betanx)

        py1 (p/point 0 1 0)
        [alphay1 betay1] (p/p3d->angles py1)
        py1t (p/angles->p3d alphay1 betay1)

        pny (p/point 0 -1 0)
        [alphany betany] (p/p3d->angles pny)
        pnyt (p/angles->p3d alphany betany)

        pz1 (p/point 0 1 0)
        [alphaz1 betaz1] (p/p3d->angles pz1)
        pz1t (p/angles->p3d alphaz1 betaz1)

        pnz (p/point 0 0 -1)
        [alphanz betanz] (p/p3d->angles pnz)
        pnzt (p/angles->p3d alphanz betanz)

        p (p/normalize (p/point 3 2 4))
        [alpha beta] (p/p3d->angles p)
        pt (p/angles->p3d alpha beta) ]
    (is (p/close? px1 px1t))
    (is (p/close? pnx pnxt))
    (is (p/close? py1 py1t))
    (is (p/close? pny pnyt))
    (is (p/close? pz1 pz1t))
    (is (p/close? pnz pnzt))
    (is (p/close? p pt))))
