(ns sanakan.mathematics.voronoi
  (:use [sanakan.mathematics.geometry]))

;; a function for creating a binary search tree from a map.
(defn compare-key-fn [key1 key2] (< (:y key1) (:y key2)))

(defn voronoi
  "Create a voronoi diagram for a couple of sites."
  [sites]
  (let [tree (reduce conj (sorted-set-by compare-key-fn) sites)]
    tree
    )
  )
