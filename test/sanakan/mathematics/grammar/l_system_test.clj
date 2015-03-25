(ns sanakan.mathematics.grammar.l-system-test
  (:require
    [sanakan.mathematics.grammar.l-system :as ls]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.commons :as c])
  (:use midje.sweet))

;; test the algae system
(def algae-rule1 (ls/rule :a '(:a :b)))
(def algae-rule2 (ls/rule :b '(:a)))
(def algae-rules (list algae-rule1 algae-rule2))
(def algae (ls/lsystem :a algae-rules))
(fact (:state algae) => '(:a))
(def algae-product-1 (ls/produce algae))
(fact (:state algae-product-1) => '(:a :b))
(def algae-product-2 (ls/produce algae-product-1))
(fact (:state algae-product-2) => '(:a :b :a))
(def algae-product-3 (ls/produce algae-product-2))
(fact (:state algae-product-3) => '(:a :b :a :a :b))

;; test the Koch curve
(def koch-rule1 (ls/rule :F '(:F :+ :F :- :F :- :F :+ :F)))
(def koch-rules (list koch-rule1))
(def koch (ls/lsystem :F koch-rules))
(fact (:state koch) => '(:F))
(def koch-product-1 (ls/produce koch))
(fact (:state koch-product-1) => '(:F :+ :F :- :F :- :F :+ :F))
(def koch-product-2 (ls/produce koch-product-1))
(fact (:state koch-product-2) => '(:F :+ :F :- :F :- :F :+ :F :+ :F :+ :F :- :F :- :F
                                      :+ :F :- :F :+ :F :- :F :- :F :+ :F :- :F :+ :F
                                      :- :F :- :F :+ :F :+ :F :+ :F :- :F :- :F :+ :F))

(defn update-angle
  [render-state sym]
  (cond
    (= :+ sym) (+ (:angle render-state) (/ java.lang.Math/PI 4))
    (= :- sym) (- (:angle render-state) (/ java.lang.Math/PI 4))
    :else (:angle render-state)))

(defn update-points
  [render-state sym]
  (if (= :F sym)
    (conj (:points render-state) (p/point-at (first (:points render-state)) (:angle render-state) (:length render-state)))
    (:points render-state)))

(def rendered-points (ls/render-points koch-product-1 (p/point 0 0) 1 (ls/renderer update-points update-angle)))

(fact (count rendered-points) => 6)
(fact (c/close-to (p/distance (nth rendered-points 0) (p/point 0     0))     0) => true)
(fact (c/close-to (p/distance (nth rendered-points 1) (p/point 1     0))     0) => true)
(fact (c/close-to (p/distance (nth rendered-points 2) (p/point 1.707 0.707)) 0) => true)
(fact (c/close-to (p/distance (nth rendered-points 3) (p/point 2.707 0.707)) 0) => true)
(fact (c/close-to (p/distance (nth rendered-points 4) (p/point 3.414 0))     0) => true)
(fact (c/close-to (p/distance (nth rendered-points 5) (p/point 4.414 0))     0) => true)

(def rendering (ls/render koch-product-1 (p/point 0 0) 1 (ls/renderer update-points update-angle)))
(fact (count rendering) => 5)
(fact (nth rendering 0) => (l/line (nth rendered-points 0) (nth rendered-points 1)))
(fact (nth rendering 1) => (l/line (nth rendered-points 1) (nth rendered-points 2)))
(fact (nth rendering 2) => (l/line (nth rendered-points 2) (nth rendered-points 3)))
(fact (nth rendering 3) => (l/line (nth rendered-points 3) (nth rendered-points 4)))
(fact (nth rendering 4) => (l/line (nth rendered-points 4) (nth rendered-points 5)))

(defn outline-half
  [rendering dist]
  (let [mapped (map #(l/parallel % dist) rendering)
        intersections (map #(l/intersect-sloped %1 %2) (butlast mapped) (rest mapped)) 
        points (concat (list (:p1 (first mapped))) intersections (list (:p2 (last mapped))))]
    (ls/connect-points points)))

(defn outline
  [rendering thickness]
  (let [upper (outline-half rendering (/ thickness 2))
        lower (outline-half rendering (* -1 (/ thickness 2)))]
    {:upper upper :lower lower}))

(def upper-half (outline-half rendering 1))
(fact (count upper-half) => 5)
(fact (l/parallel? (nth upper-half 0) (nth rendering 0)) => true)
(fact (l/parallel? (nth upper-half 1) (nth rendering 1)) => true)
(fact (l/parallel? (nth upper-half 2) (nth rendering 2)) => true)
(fact (l/parallel? (nth upper-half 3) (nth rendering 3)) => true)
(fact (l/parallel? (nth upper-half 4) (nth rendering 4)) => true)
