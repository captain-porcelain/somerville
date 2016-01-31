(ns sanakan.mathematics.geometry.fortune
  (:require
    [clojure.zip :as z]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.parabola :as par]
    [sanakan.mathematics.geometry.point :as p]))

;; define a site event type.
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

(defrecord TreeNode [event parabola left right]
  c/Printable
  (c/out [this i keepfirst]
    (str (when keepfirst (c/indent i)) "Node for " (c/out event) " has "
         (if (nil? parabola) "no parabola yet " (c/out parabola)) "\n"
         (c/indent (+ i 2)) "left:  " (if (nil? left)  "-" (c/out left  (+ i 2) false)) "\n"
         (c/indent (+ i 2)) "right: " (if (nil? right) "-" (c/out right (+ i 2) false))))
  (c/out [this i] (c/out this i true))
  (c/out [this] (c/out this 0)))

(defn treenode
  "Create a new TreeNode"
  ([event parabola]
   (TreeNode. event parabola nil nil))
  ([event parabola left right]
   (TreeNode. event parabola left right)))

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

(defn leaf?
  "Is this TreeNode a leaf?"
  [node]
  (and (nil? (:left node)) (nil? (:right node))))

(defn make-zipper
  "Create a zipper from the root tree node."
  [root]
  (z/zipper
    #(not (leaf? %))
    #(list (:left %) (:right %))
    (fn [node children] (TreeNode. (:event node) (:parabola node) (first children) (second children)))
    root))

(defn find-parabola
  "Find the parabola that is to be split in two by a site event."
  [zipper event]
  (if (leaf? (z/node zipper))
    zipper
    (if (< (:x (:point event)) (:x (:point (:event (z/node zipper)))))
      (recur (z/down zipper) event)
      (recur (z/right (z/down zipper)) event))))

(defn create-subtree
  [node event]
  (treenode (:event node) nil
              (treenode (:event node) nil
                (treenode (:event node) nil nil nil)
                (treenode event nil nil nil))
              (treenode (:event node) nil nil nil)))

(defn edit-tree
  [tree event]
  (z/root (z/edit
                (find-parabola (make-zipper tree) event)
                (fn [n] (create-subtree n event)))))

(defn add-parabola
  [tree event]
  (if (nil? tree)
    (treenode event nil)
    (if (leaf? tree)
      (treenode (:event tree) nil
                (treenode (:event tree) nil)
                (treenode event nil))
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
