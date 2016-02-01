(ns sanakan.mathematics.geometry.fortune
  (:require
    [clojure.zip :as z]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.parabola :as par]
    [sanakan.mathematics.geometry.point :as p]))

;; ==============================================================================================================
;; define a site event type from a point and a type. Types can be :site and :circle
(defrecord SiteEvent [point type]
  c/Printable
  (c/out [this i] (str (c/indent i) "Event " type " at " (c/out point)))
  (c/out [this] (c/out this 0)))

(defn event
  "Create a new event for a point p with type t."
  [p t]
  (SiteEvent. p t))

(defn sort-events
  "Sort events by y position."
  [events]
  (sort-by #(:y (:point %)) events))

(defn events
  "Create sorted events for a list of points."
  [points]
  (sort-events (map #(event % :site) points)))

;; ==============================================================================================================
;; define the structure for tracking edges of the voronoi diagram
(defrecord Edge [start left right end]
  c/Printable
  (c/out [this i] (str (c/indent i) "Edge start: " (c/out start) " end: "
                       (if (nil? end) "-" (c/out end)) " left: " (c/out left) " right: " (c/out right)))
  (c/out [this] (c/out this 0)))

(defn edge
  ([start left right]
   "Create an unfinished edge from a start point having left and right defining points."
   (Edge. start left right nil))
  ([p1 p2]
   "Create an unfinished edge from two points by calculating the start at 0 y."
   (let [lp (if (< (:x p1) (:x p2)) p1 p2)
         rp (if (< (:x p1) (:x p2)) p2 p1)
         s (p/point (/ (+ (:x p1) (:x p2)) 2) 0)]
     (edge s lp rp))))

;; ==============================================================================================================
;; define records and functions for managing the binary search tree. 
(defn leaf?
  "Is this TreeNode a leaf?"
  [node]
  (and (nil? (:left node)) (nil? (:right node))))

(defrecord TreeNode [event edge left right]
  c/Printable
  (c/out [this i keepfirst]
    (str (when keepfirst (c/indent i)) "Node for " (c/out event) " has "
         (if (nil? edge) "no edge yet " (str "\n" (c/out edge (+ i 2))))
         (if (not (leaf? this))
           (str "\n"
             (c/indent (+ i 2)) "left:  " (if (nil? left)  "-" (c/out left  (+ i 2) false)) "\n"
             (c/indent (+ i 2)) "right: " (if (nil? right) "-" (c/out right (+ i 2) false))))))
  (c/out [this i] (c/out this i true))
  (c/out [this] (c/out this 0)))

(defn treenode
  "Create a new TreeNode"
  ([event]
   (TreeNode. event nil nil nil))
  ([event edge]
   (TreeNode. event edge nil nil))
  ([event edge left right]
   (TreeNode. event edge left right)))

(defn make-zipper
  "Create a zipper from the root tree node."
  [root]
  (z/zipper
    #(not (leaf? %))
    #(list (:left %) (:right %))
    (fn [node children] (treenode (:event node) (:edge node) (first children) (second children)))
    root))

(defn right-leaf
  "Find the child of the node that is a leaf on the right side. follow is used to track if we should be stepping to the left child."
  [node follow?]
  (if (leaf? node)
    node
    (if follow?
      (recur (:left  node) true)
      (recur (:right node) true))))

(defn left-leaf
  "Find the child of the node that is a leaf on the left side. follow is used to track if we should be stepping to the right child."
  [node follow?]
  (if (leaf? node)
    node
    (if follow?
      (recur (:right node) true)
      (recur (:left  node) true))))

;; ==============================================================================================================
;; define the data structure we need to represent a voronoi diagram.
(defrecord Voronoi [points events tree step]
  c/Printable
  (c/out [this i] (str (c/indent i) "Voronoi step " step " for the points\n"
                       (reduce str (interpose "\n" (for [p points] (c/out p (+ i 2)))))
                       "\n\nconsists of queued events\n"
                       (reduce str (interpose "\n" (for [e events] (c/out e (+ i 2)))))
                       (if (nil? tree)
                         "\n\nand no tree"
                         (str "\n\nand the tree\n" (c/out tree (+ i 2))))
                       "\n\n"))
  (c/out [this] (c/out this 0)))

(defn find-parabola
  "Find the parabola that is to be split in two by a site event."
  [zipper event]
  (if (leaf? (z/node zipper))
    zipper
    (if (< (:x (:point event)) (:x (:point (:event (z/node zipper)))))
      (recur (z/down zipper) event)
      (recur (z/right (z/down zipper)) event))))

(defn start-of-edge
  "Define the starting point a new edge."
  [node event]
  (p/point
    (:x (:point event))
    (par/solve-parabola-at
      (par/parabola-from-focuspoint-and-directrix
        (:point (:event node))
        (l/line (p/point 0 (:y (:point event))) (p/point 1 (:y (:point event)))))
      (:x (:point event)))))

(defn create-subtree
  "Create TreeNode structure for representing a new site event."
  [node event]
  (let [tmp (dorun (println "test"))
        start (start-of-edge node event)
        tmp (dorun (println (c/out start)))]
    (treenode (:event node)
              (edge start (:point event) (:point (:event node)))
              (treenode (:event node)
                (edge start (:point (:event node)) (:point event))
                (treenode (:event node))
                (treenode event))
              (treenode (:event node)))))

(defn edit-tree
  "Use zipper to append sub tree for new site event."
  [tree event]
  (z/root (z/edit
                (find-parabola (make-zipper tree) event)
                (fn [n] (create-subtree n event)))))

(defn add-parabola
  [tree event]
  (if (nil? tree)
    (treenode event)
    (if (leaf? tree)
      (treenode (:event tree)
                (edge (:point (:event tree)) (:point event))
                (treenode (:event tree))
                (treenode event))
      (edit-tree tree event))))

(defn remove-parabola
  [tree point]
  tree)

(defn step
  "Process first event in events and update event queue and tree."
  [v]
  (let [e (first (:events v))
        tree (:tree v)]
    (Voronoi.
      (:points v)
      (rest (:events v))
      (if (= :site (:type e))
        (add-parabola tree e)
        (remove-parabola tree e))
      (+ 1 (:step v)))))

(defn voronoi
  [points]
  (Voronoi. points (events points) nil 0))
