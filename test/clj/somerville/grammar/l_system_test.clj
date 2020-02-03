(ns somerville.grammar.l-system-test
  (:require
    [somerville.grammar.l-system :as ls]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.commons :as c])
  (:use clojure.test))

;; test the algae system
(deftest algae-system
  (let [algae-rule1 (ls/rule :a '(:a :b))
        algae-rule2 (ls/rule :b '(:a))
        algae-rules (list algae-rule1 algae-rule2)
        algae (ls/lsystem :a algae-rules)
        algae-product-1 (ls/produce algae)
        algae-product-2 (ls/produce algae-product-1)
        algae-product-3 (ls/produce algae-product-2)]
    (is (= (:state algae) '(:a)))
    (is (= (:state algae-product-1) '(:a :b)))
    (is (= (:state algae-product-2) '(:a :b :a)))
    (is (= (:state algae-product-3) '(:a :b :a :a :b)))))

;; test the Koch curve
(deftest koch-curve-1
  (let [koch-rule1 (ls/rule :F '(:F :+ :F :- :F :- :F :+ :F))
        koch-rules (list koch-rule1)
        koch (ls/lsystem :F koch-rules)
        koch-product-1 (ls/produce koch)
        koch-product-2 (ls/produce koch-product-1)]
    (is (= (:state koch) '(:F)))
    (is (= (:state koch-product-1) '(:F :+ :F :- :F :- :F :+ :F)))
    (is (= (:state koch-product-2) '(:F :+ :F :- :F :- :F :+ :F :+ :F :+ :F :- :F :- :F
                                     :+ :F :- :F :+ :F :- :F :- :F :+ :F :- :F :+ :F
                                     :- :F :- :F :+ :F :+ :F :+ :F :- :F :- :F :+ :F)))))

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

(deftest render-koch
  (let [koch-rule1 (ls/rule :F '(:F :+ :F :- :F :- :F :+ :F))
        koch-rules (list koch-rule1)
        koch (ls/lsystem :F koch-rules)
        koch-product-1 (ls/produce koch)
        rendered-points (ls/render-points koch-product-1 (p/point 0 0) 1 (ls/renderer update-points update-angle))
        rendering (ls/render koch-product-1 (p/point 0 0) 1 (ls/renderer update-points update-angle))
        upper-half (outline-half rendering 1)]
    (is (= (count rendered-points) 6))
    (is (= (c/close-to (p/distance (nth rendered-points 0) (p/point 0     0))     0) true))
    (is (= (c/close-to (p/distance (nth rendered-points 1) (p/point 1     0))     0) true))
    (is (= (c/close-to (p/distance (nth rendered-points 2) (p/point 1.707 0.707)) 0) true))
    (is (= (c/close-to (p/distance (nth rendered-points 3) (p/point 2.707 0.707)) 0) true))
    (is (= (c/close-to (p/distance (nth rendered-points 4) (p/point 3.414 0))     0) true))
    (is (= (c/close-to (p/distance (nth rendered-points 5) (p/point 4.414 0))     0) true))

    (is (= (count rendering) 5))
    (is (= (nth rendering 0) (l/line (nth rendered-points 0) (nth rendered-points 1))))
    (is (= (nth rendering 1) (l/line (nth rendered-points 1) (nth rendered-points 2))))
    (is (= (nth rendering 2) (l/line (nth rendered-points 2) (nth rendered-points 3))))
    (is (= (nth rendering 3) (l/line (nth rendered-points 3) (nth rendered-points 4))))
    (is (= (nth rendering 4) (l/line (nth rendered-points 4) (nth rendered-points 5))))

    (is (= (count upper-half) 5))
    (is (= (l/parallel? (nth upper-half 0) (nth rendering 0)) true))
    (is (= (l/parallel? (nth upper-half 1) (nth rendering 1)) true))
    (is (= (l/parallel? (nth upper-half 2) (nth rendering 2)) true))
    (is (= (l/parallel? (nth upper-half 3) (nth rendering 3)) true))
    (is (= (l/parallel? (nth upper-half 4) (nth rendering 4)) true))))
