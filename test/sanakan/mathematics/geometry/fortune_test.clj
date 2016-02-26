(ns sanakan.mathematics.geometry.fortune-test
  (:require
    [clojure.zip :as z]
    [clojure.pprint :as pp]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.fortune :as f]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; ==============================================================================================================
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

;; ==============================================================================================================
;; Test tree navigation
;; define a test tree
;;                            (1,1)
;;                   /                     \
;;                (2,1)                  (2,2)
;;            /          \            /         \
;;         (3,1)       (3,2)       (3,3)      (3,4)
;;        /    \      /    \      /    \      /    \
;;     (4,1) (4,2) (4,3) (4,4) (4,5) (4,6) (4,7) (4,8)
;;    /    \
;; (5,1) (5,2)

(def n5-1 (f/treenode (f/event (p/point 5 1) :site)))
(def n5-2 (f/treenode (f/event (p/point 5 2) :site)))

(def n4-1 (f/treenode (f/event (p/point 4 1) :site) nil n5-1 n5-2))
(def n4-2 (f/treenode (f/event (p/point 4 2) :site)))
(def n4-3 (f/treenode (f/event (p/point 4 3) :site)))
(def n4-4 (f/treenode (f/event (p/point 4 4) :site)))
(def n4-5 (f/treenode (f/event (p/point 4 5) :site)))
(def n4-6 (f/treenode (f/event (p/point 4 6) :site)))
(def n4-7 (f/treenode (f/event (p/point 4 7) :site)))
(def n4-8 (f/treenode (f/event (p/point 4 8) :site)))

(def n3-1 (f/treenode (f/event (p/point 3 1) :site) nil n4-1 n4-2))
(def n3-2 (f/treenode (f/event (p/point 3 2) :site) nil n4-3 n4-4))
(def n3-3 (f/treenode (f/event (p/point 3 3) :site) nil n4-5 n4-6))
(def n3-4 (f/treenode (f/event (p/point 3 4) :site) nil n4-7 n4-8))

(def n2-1 (f/treenode (f/event (p/point 2 1) :site) nil n3-1 n3-2))
(def n2-2 (f/treenode (f/event (p/point 2 2) :site) nil n3-3 n3-4))

(def n1-1 (f/treenode (f/event (p/point 1 1) :site) nil n2-1 n2-2))

(fact (z/node (f/left-leaf  (f/make-zipper n1-1))) => n4-4)
(fact (z/node (f/right-leaf (f/make-zipper n1-1))) => n4-5)
(fact (z/node (f/left-leaf  (f/make-zipper n3-1))) => n5-2)
(fact (z/node (f/right-leaf (f/make-zipper n3-1))) => n4-2)

(fact (f/right-parent (z/right (z/down (f/make-zipper n1-1)))) => nil)
(fact (z/node (f/right-parent (z/right (z/down (z/down (f/make-zipper n1-1)))))) => n1-1)
(fact (f/left-parent (z/down (f/make-zipper n1-1))) => nil)
(fact (z/node (f/left-parent (z/right (z/down (f/make-zipper n1-1))))) => n1-1)

;; ==============================================================================================================
;; Test edge handling
(def edge1 (f/edge (p/point 1 4) (p/point 1 2) (p/point 3 4)))
(def edge2 (f/edge (p/point 5 3) (p/point 4 3) (p/point 5 2)))
(def edge3 (f/edge (p/point 5 4) (p/point 4 4) (p/point 5 3)))
(def i-edge1-edge2 (f/edge-intersection edge1 edge2))
(fact i-edge1-edge2 => (p/point 7/2 3/2))
(def i-edge2-edge3 (f/edge-intersection edge2 edge3))
(fact i-edge2-edge3 => nil)

;; ==============================================================================================================
;; Check creation of voronois
(def voronoi1-step0 (f/voronoi points1))
(dorun (println (c/out voronoi1-step0)))
(def voronoi1-step1 (f/step voronoi1-step0))
(dorun (println (c/out voronoi1-step1)))
(def voronoi1-step2 (f/step voronoi1-step1))
(dorun (println (c/out voronoi1-step2)))
(def voronoi1-step3 (f/step voronoi1-step2))
(dorun (println (c/out voronoi1-step3)))
