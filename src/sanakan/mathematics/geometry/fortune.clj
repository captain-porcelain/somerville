(ns sanakan.mathematics.geometry.fortune
  (:require
    [clojure.zip :as z]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.parabola :as par]
    [sanakan.mathematics.geometry.point :as p]))

;; ==============================================================================================================
;; Define a site event type from a point and a type. Types can be :site and :circle
(defrecord SiteEvent [point type]
  c/Printable
  (c/out [this i] (str (c/indent i) "Event " type " at " (c/out point)))
  (c/out [this] (c/out this 0)))

(defn event
  "Create a new event for a point p with type t."
  [p t]
  (SiteEvent. p t))

(defn sort-points
  "Sort points by y position."
  [points]
  (sort-by #(:y %) points))

(defn sort-events
  "Sort events by y position."
  [events]
  (sort-by #(:y (:point %)) events))

(defn events
  "Create sorted events for a list of points."
  [points]
  (sort-events (map #(event % :site) points)))

;; ==============================================================================================================
;; Define the structure for tracking edges of the voronoi diagram
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
   "Create an unfinished edge from two points by calculating the start at y = 0."
   (let [lp (if (< (:x p1) (:x p2)) p1 p2)
         rp (if (< (:x p1) (:x p2)) p2 p1)
         s (p/point (/ (+ (:x p1) (:x p2)) 2) 0)]
     (edge s lp rp))))

;; ==============================================================================================================
;; Define records and functions for managing a binary search tree.
;; Each tree node contains one site event and represents a part of a parabola for that event.
;; Nodes also contain the edges separating voronoi cells.
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
  "Find the child of the current zipper position that is a leaf on the right side.
  The follow flag is used to track if we should be stepping to the left child."
  ([zipper follow?]
   (if (or (nil? zipper) (leaf? (z/node zipper)))
     zipper
     (if follow?
       (recur (z/down zipper) true)
       (recur (z/right (z/down zipper)) true))))
  ([zipper]
   (right-leaf zipper false)))

(defn left-leaf
  "Find the child of the current zipper position that is a leaf on the left side.
  The follow flag is used to track if we should be stepping to the right child."
  ([zipper follow?]
   (if (or (nil? zipper) (leaf? (z/node zipper)))
     zipper
     (if follow?
       (recur (z/right (z/down zipper)) true)
       (recur (z/down zipper) true))))
  ([zipper]
   (left-leaf zipper false)))

(defn right-parent
  "Find the parent of the current zipper position so that the current node is not a right child."
  [zipper]
  (let [parent (z/up zipper)]
    (if (nil? parent)
      nil
      (if (= (:right (z/node parent)) (z/node zipper))
        (recur parent)
        parent))))

(defn left-parent
  "Find the parent of the current zipper position so that the current node is not a left child."
  [zipper]
  (let [parent (z/up zipper)]
    (if (nil? parent)
      nil
      (if (= (:left (z/node parent)) (z/node zipper))
        (recur parent)
        parent))))

(defn tree-seq
  "Create a sequence of the tree."
  [tree]
  (map z/node (take-while (complement z/end?) (iterate z/next (make-zipper tree)))))

;; ==============================================================================================================
;; Define the data structure we need to represent a voronoi diagram.
;; It contains all the inital points and the event queue created from those points.
;; The tree is initially empty and step is set to 0.
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
  "Find the parabola that is to be split in two by a site event. This is done by walking down
  the tree, calculating the parabolas for each child node and intersecting the parabolas.
  Depending if the event is left or right of the intersection the child node is selected for
  continuing the search until a leaf is found."
  [zipper event]
  (if (leaf? (z/node zipper))
    zipper
    (let [left-point  (:point (:event (z/node (left-leaf  zipper))))
          right-point (:point (:event (z/node (right-leaf zipper))))
          event-point (:point event)
          parabola-left (par/parabola-from-focuspoint-and-directrix-y left-point (:y event-point))
          parabola-right (par/parabola-from-focuspoint-and-directrix-y right-point (:y event-point))
          parabola (par/subtract parabola-left parabola-right)
          zeros (par/find-zero-of-parabola parabola)
          x (if (< (:y left-point) (:y right-point)) (second zeros) (first zeros))]
      (if (< (:x (:point event)) x)
        (recur (z/down zipper) event)
        (recur (z/right (z/down zipper)) event)))))

(defn check-circle
  ;; TODO use
  ""
  [zipper]
  (let [lp (left-parent zipper)
        rp (right-parent zipper)
        ll (left-leaf lp)
        rl (right-leaf rp)]
    ;; TODO finish implementation
    (if (or (nil? ll) (nil? rl) (= (:point (:event ll)) (:point (:event rl))))
      nil
      nil)))

(defn start-of-edge
  "Define the starting point a new edge. It is defined by the newly split parabola's value at x."
  [parabola event]
  (p/point
    (:x (:point event))
    (par/solve-parabola-at
      (par/parabola-from-focuspoint-and-directrix-y
        (:point (:event (z/node parabola)))
        (:y (:point event)))
      (:x (:point event)))))

(defn create-subtree
  "Create TreeNode structure for representing a new site event.
  If p1 is the parabola that is to be split the created tree is as follows:
                 (p1)
               //    \\
             (p1)    (p1)
            //   \\
          (p1)   (p2)"
  [node event parabola]
  (let [start (start-of-edge parabola event)]
    (treenode (:event node)
              (edge start (:point event) (:point (:event node)))
              (treenode (:event node)
                        (edge start (:point (:event node)) (:point event))
                        (treenode (:event node))
                        (treenode event))
              (treenode (:event node)))))

(defn new-nodes
  "From the zipper where new nodes have been added get the zippers moved to these nodes."
  [zipper]
  (list
    (z/down  (z/down zipper))
    (z/right (z/down zipper))))

(defn add-parabola
  "A new site event causes the tree to be updated and returns the new nodes to check for circle events."
  [tree event]
  (if (nil? tree)
    [(treenode event) (list)]
    (let [parabola (find-parabola (make-zipper tree) event)
          updated (z/edit parabola (fn [n] (create-subtree n event parabola)))]
      [(z/root updated) (new-nodes updated)])))

(defn remove-parabola
  [tree point]
  [tree (list)])

(defn step
  "Process first event in events and update event queue and tree."
  [v]
  (let [e (first (:events v))
        events (rest (:events v))
        tree (:tree v)
        [new-tree new-nodes] (if (= :site (:type e))
                               (add-parabola tree e)
                               (remove-parabola tree e))
        new-events (filter #(not (nil? %)) (map check-circle new-nodes))]
    (Voronoi.
      (:points v)
      (sort-events (concat events new-events))
      new-tree
      (+ 1 (:step v)))))

(defn voronoi
  [points]
  (Voronoi. (sort-points points) (events points) nil 0))
