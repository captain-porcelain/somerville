(ns sanakan.mathematics.voronoi
  (:require [sanakan.mathematics.geometry.geometry :as g]))

;; a function for creating a binary search tree from a map.
(defn compare-key-fn [key1 key2] (< (:y key1) (:y key2)))

(defn set-as-tree
  "Use the set as a tree with a given sorting function."
  [sites] (reduce conj (sorted-set-by compare-key-fn) sites))

(defn locate-arc-above
  [sites sweepline x]
  (:s (first
        (sort-by
          #(:y %)
          (map #({:s % :y (g/solve-parabola-at % x)})
               (map #(g/parabola-from-focuspoint-and-directrix % sweepline) sites))))))

(defn sort-sites
  "Sort sites by their x coordinate."
  [sites]
  (sort-by #(:y %) sites))

(defn walk-leafs
  [node]
  (let [l (if (nil? (:left node)) (list) (walk-leafs (:left node)))
        r (if (nil? (:right node)) (list) (walk-leafs (:right node)))
        n (if (and (nil? (:left node)) (nil? (:right node))) (list node) (list))]
    (flatten
      (list l r n))))

(defn sites
  "Get the sites in the tree from left to right."
  [tree]
  (map #(:site %) (walk-leafs tree)))

(defn add-site
  "Add a new site to the arc tree"
  [tree site y]
  (if (nil? tree)
    {:left nil :right nil :site site}
    (let [s (locate-arc-above)])))

(defn voronoi
  "Create a voronoi diagram for a couple of sites."
  [sites]
  (let [events (sort-sites sites)]
    {:events events :sites events}
    ))
