(ns somerville.geometry.fortune-test
  (:require
    [clojure.zip :as z]
    [clojure.pprint :as pp]
    [somerville.geometry.commons :as c]
    [somerville.geometry.fortune :as f]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

;; ==============================================================================================================
;; check creation and sorting of events
(deftest events
    (let [point1 (p/point 2 2)
          event1 (f/event point1 :site)
          point2 (p/point 1 3)
          point3 (p/point 1 1)
          points1 (list point1 point2 point3)
          events1 (f/events points1)]
      (is (= (:point event1) point1))
      (is (= (count events1) 3))
      (is (= (:point (nth events1 0)) point3))
      (is (= (:point (nth events1 1)) point1))
      (is (= (:point (nth events1 2)) point2))))

;; ==============================================================================================================
;; Test edge handling
(deftest edges
  (let [edge1 (f/edge (p/point 1 4) (p/point 1 2) (p/point 3 4))
        edge2 (f/edge (p/point 5 3) (p/point 4 3) (p/point 5 2))
        edge3 (f/edge (p/point 5 4) (p/point 4 4) (p/point 5 3))
        i-edge1-edge2 (f/edge-intersection edge1 edge2)
        i-edge2-edge3 (f/edge-intersection edge2 edge3)]
    (is (= i-edge1-edge2 (p/point 7/2 3/2)))
    (is (= i-edge2-edge3 nil))
    (is (= (f/start-of-edge (p/point 2 3) (f/event (p/point 2 1) :site)) (p/point 2 2)))))

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

(deftest tree-navigation
  (let [n5-1 (f/treenode (f/event (p/point 5 1) :site))
        n5-2 (f/treenode (f/event (p/point 5 2) :site))
        n4-1 (f/treenode (f/event (p/point 4 1) :site) nil n5-1 n5-2)
        n4-2 (f/treenode (f/event (p/point 4 2) :site))
        n4-3 (f/treenode (f/event (p/point 4 3) :site))
        n4-4 (f/treenode (f/event (p/point 4 4) :site))
        n4-5 (f/treenode (f/event (p/point 4 5) :site))
        n4-6 (f/treenode (f/event (p/point 4 6) :site))
        n4-7 (f/treenode (f/event (p/point 4 7) :site))
        n4-8 (f/treenode (f/event (p/point 4 8) :site))
        n3-1 (f/treenode (f/event (p/point 3 1) :site) nil n4-1 n4-2)
        n3-2 (f/treenode (f/event (p/point 3 2) :site) nil n4-3 n4-4)
        n3-3 (f/treenode (f/event (p/point 3 3) :site) nil n4-5 n4-6)
        n3-4 (f/treenode (f/event (p/point 3 4) :site) nil n4-7 n4-8)
        n2-1 (f/treenode (f/event (p/point 2 1) :site) nil n3-1 n3-2)
        n2-2 (f/treenode (f/event (p/point 2 2) :site) nil n3-3 n3-4)
        n1-1 (f/treenode (f/event (p/point 1 1) :site) nil n2-1 n2-2)]

    (is (= (f/leaf? n5-1) true))
    (is (= (f/leaf? n4-1) false))

    (is (= (z/node (f/left-leaf  (f/make-zipper n1-1))) n4-4))
    (is (= (z/node (f/right-leaf (f/make-zipper n1-1))) n4-5))
    (is (= (z/node (f/left-leaf  (f/make-zipper n3-1))) n5-2))
    (is (= (z/node (f/right-leaf (f/make-zipper n3-1))) n4-2))

    (is (= (f/right-parent (z/right (z/down (f/make-zipper n1-1)))) nil))
    (is (= (z/node (f/right-parent (z/right (z/down (z/down (f/make-zipper n1-1)))))) n1-1))
    (is (= (f/left-parent (z/down (f/make-zipper n1-1))) nil))
    (is (= (z/node (f/left-parent (z/right (z/down (f/make-zipper n1-1))))) n1-1))

    (is (= (:point (:event (z/node (f/find-parabola-for-insert (f/make-zipper n5-1) (f/event (p/point 2 1) :site))))) (p/point 5 1)))))

;; test building the tree and finding parabolas
(deftest tree-building
  (let [e1 (f/event (p/point 3 2) :site)
        e2 (f/event (p/point 2 4) :site)
        e3 (f/event (p/point 5 5) :site)
        e4 (f/event (p/point 4 7) :site)
        tree1-1 (first (f/add-parabola nil e1))
        new-nodes1-1 (second (f/add-parabola nil e1))
        tree1-2 (first (f/add-parabola tree1-1 e2))
        new-nodes1-2 (second (f/add-parabola tree1-1 e2))
        tree1-3 (first (f/add-parabola tree1-2 e3))]

    ;; first the tree looks like this:
    ;; (3,2)
    (is (= (count (f/sequence-from-tree tree1-1)) 1))
    (is (= (count new-nodes1-1) 0))

    ;; then it becomes
    ;;        (3,2)
    ;;      //     \\
    ;;    (3,2)    (3,2)
    ;;   //   \\
    ;; (3,2)   (2,4)
    (is (= (count (f/sequence-from-tree tree1-2)) 5))
    (is (= (count new-nodes1-2) 2))
    (is (= (z/node (first  new-nodes1-2)) (z/node (z/down  (z/down (f/make-zipper tree1-2))))))
    (is (= (z/node (second new-nodes1-2)) (z/node (z/right (z/down (f/make-zipper tree1-2))))))

    ;; adding the third event makes it:
    ;;           (3,2)
    ;;       //        \\
    ;;    (3,2)         (3,2)
    ;;   //   \\       //   \\
    ;; (3,2)   (2,4) (3,2)   (3,2)
    ;;              //  \\
    ;;            (3,2) (5,5)
    (is (= (f/find-parabola-for-insert (f/make-zipper tree1-2) e3) (z/right (z/down (f/make-zipper tree1-2)))))
    (is (= (count (f/sequence-from-tree tree1-3)) 9))))

;; ==============================================================================================================
;; Check creation of voronois
;(def voronoi1-step0 (f/voronoi points1))
;(log/info (c/out voronoi1-step0))
;(def voronoi1-step1 (f/step voronoi1-step0))
;(log/info (c/out voronoi1-step1))
;(def voronoi1-step2 (f/step voronoi1-step1))
;(log/info (c/out voronoi1-step2))
;(def voronoi1-step3 (f/step voronoi1-step2))
;(log/info (c/out voronoi1-step3))
