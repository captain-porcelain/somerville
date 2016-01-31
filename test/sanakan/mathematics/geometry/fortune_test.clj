(ns sanakan.mathematics.geometry.fortune-test
  (:require
    [clojure.zip :as z]
    [clojure.pprint :as pp]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.fortune :as f]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; check creation and sorting of events
(def point1 (p/point 2 2))
(def event1 (f/event point1 :site))
(fact (:point event1) => point1)

(def point2 (p/point 1 3))
(def point3 (p/point 1 1))

(def points1 (list point1 point2 point3))
(def events1 (f/events points1))

(fact (count events1) => 3)
(fact (:point (nth events1 0)) => point3)
(fact (:point (nth events1 1)) => point1)
(fact (:point (nth events1 2)) => point2)

(def voronoi1-step0 (f/voronoi points1))
(dorun (println (c/out voronoi1-step0)))
(def voronoi1-step1 (f/step voronoi1-step0))
(dorun (println (c/out voronoi1-step1)))
(def voronoi1-step2 (f/step voronoi1-step1))
(dorun (println (c/out voronoi1-step2)))
(def voronoi1-step3 (f/step voronoi1-step2))
(dorun (println (c/out voronoi1-step3)))

;(def zip (f/make-zipper (:tree voronoi1-step1)))
;(dorun (println zip))
;(dorun (println (f/find-parabola zip 1)))

;(def c1 (f/treenode (f/event (p/point 1 1) :site) nil nil nil))
;(def c2 (f/treenode (f/event (p/point 4 4) :site) nil nil nil))
;(def r1 (f/treenode (f/event (p/point 2 2) :size) nil c1  c2))
;(def zip1 (f/make-zipper r1))
;(dorun (println (c/out (z/node (f/find-parabola zip1 (f/event (p/point 3.5 0) :site))))))

;(def c2c1 (f/treenode (f/event (p/point 3 3) :site) nil nil nil))
;(def c2c2 (f/treenode (f/event (p/point 5 5) :site) nil nil nil))
;(def c22  (f/treenode (f/event (p/point 4 4) :site) nil c2c1 c2c2))
;(def r2   (f/treenode (f/event (p/point 2 2) :size) nil c1  c22))

;(def zip2 (f/make-zipper r2))
;(dorun (println (c/out (z/node (f/find-parabola zip2 (f/event (p/point 3.5 0) :site))))))

;(def zip3 (f/make-zipper (z/root (z/edit (-> zip1 z/down z/right) (fn [n] c22)))))
;(dorun (println (c/out (z/node (f/find-parabola zip3 (f/event (p/point 3.5 0) :site))))))
