;; Test the geometry functions.
(ns somerville.dungeons.discovery.rasterize-test
  (:use clojure.test)
  (:use somerville.dungeons.discovery.rasterize))

(deftest basic-circle
  (def c1 (circle 9))
  (is (= (first (first c1)) -9))
  (is (= (second (first c1)) 0))
  (is (= (first (second c1)) -9))
  (is (= (second (second c1)) 1))

  (is (> (count c1) 2)))

(deftest line-in-quadrant-1
  (def l1 (line [1 1] [9 4]))
  (is (= (first (first l1)) 1))
  (is (= (second (first l1)) 1))
  (is (= (first (last l1)) 9))
  (is (= (second (last l1)) 4)))

(deftest degenerate-line
  (def l2 (line [1 1] [1 9]))
  (is (= (first (first l2)) 1))
  (is (= (second (first l2)) 1))
  (is (= (first (last l2)) 1))
  (is (= (second (last l2)) 9)))

(deftest line-in-quadrant-3
  (def l3 (line [-1 -1] [-9 -4]))
  (is (= (first (first l3)) -1))
  (is (= (second (first l3)) -1))
  (is (= (first (last l3)) -9))
  (is (= (second (last l3)) -4)))

(deftest inverse-degenerate-line
  (def l4 (line [-1 -1] [-1 -9]))
  (is (= (first (first l4)) -1))
  (is (= (second (first l4)) -1))
  (is (= (first (last l4)) -1))
  (is (= (second (last l4)) -9)))

(deftest horizontal-line
  (def l5 (line [1 1] [9 1]))
  (is (= (first (first l5)) 1))
  (is (= (second (first l5)) 1))
  (is (= (first (last l5)) 9))
  (is (= (second (last l5)) 1)))

(deftest line-in-q2
  (def l6 (line [-1 1] [-9 1]))
  (is (= (first (first l6)) -1))
  (is (= (second (first l6)) 1))
  (is (= (first (last l6)) -9))
  (is (= (second (last l6)) 1)))

(deftest line-in-q4
  (def l7 (line [1 -1] [9 -1]))
  (is (= (first (first l7)) 1))
  (is (= (second (first l7)) -1))
  (is (= (first (last l7)) 9))
  (is (= (second (last l7)) -1)))

(deftest trees
  (def l11 (list [0 0] [0 1] [0 2]))
  (def l12 (list [0 0] [1 1] [1 2]))
  (def l13 (list [1 1] [1 1] [1 2]))
  (def l14 (list [1 1] [1 1] [1 2]))
  (def l15 (list [1 2] [1 1] [1 2]))

  (def sl1 (list l11 l12 l13 l14 l15))
  (def tree1 (insert-nodes sl1))
  (is (= (count tree1) 3))

  (def l21 (list [0 1] [0 2]))
  (def l22 (list [1 1] [1 2]))
  (def sl2 (list l21 l22))
  (def tree2 (insert-nodes sl2))
  (is (= (count tree2) 2)))

(deftest tree-list
  (defn compare-tree-to-list
    [size]
    (let [;tmp (dorun (println (str "pixels in square of size "size ": " (* (* 2 size) (* 2 size)))))
          sl (sight-lines size)
          ;tmp (dorun (println (str "pixels in list of lines of size " size ": " (count-pixels sl))))
          t (sight-lines-tree size)
          ;tmp (dorun (println (str "pixels in tree of size " size ": " (count-pixels-tree t))))
          tf (tree-full? t sl)
          ;tmp (dorun (println (str "all pixels of list of lines in tree? " tf)))
          ]
      (is (= tf true))))

  (compare-tree-to-list 10))
  ;(compare-tree-to-list 100))
