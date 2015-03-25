(ns sanakan.mathematics.visualization.l-system
  (:require [sanakan.mathematics.grammar.l-system :as ls]
            [sanakan.mathematics.geometry.point :as p]
            [sanakan.mathematics.geometry.line :as line]
            [quil.core :as quil])
  (:gen-class))

(def width 320)
(def height 320)

(def koch-rule1 (ls/rule :F '(:F :+ :F :- :F :- :F :+ :F)))
(def koch-rules (list koch-rule1))
(def koch (ls/lsystem :F koch-rules))

(def alt (atom false))
(def current (atom koch))
(def rendering (atom (list)))
(def length (atom 20))

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

(defn render
  []
  (reset! rendering (ls/render @current (p/point 10 10) @length (ls/renderer update-points update-angle))))

(defn draw-koch
  []
  (dorun
    (for [l @rendering]
      (let []
        (quil/stroke-float 255 255 0)
        (quil/fill-float 255 255 0)
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (dorun (draw-koch)))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 1))

(defn mouse-pressed [])
(defn mouse-released [])

(defn key-released []
  (if (= (quil/key-code) 18) ; alt
    (reset! alt false)))

(defn key-pressed []
  ;;(dorun (println (str "pressed code " (quil/key-code))))
  (when (= (quil/key-code) 18) ; alt
    (reset! alt true))
  (when (= (quil/key-code) 521) ; +
    (reset! length (+ @length 10)))
  (when (= (quil/key-code) 45) ; -
    (reset! length (- @length 10)))
  (if (= (quil/key-code) 86) ; v
    (reset! current (ls/produce @current))
    (render)))

(defn show []
  (quil/sketch
    :title "koch curve"
    :setup setup
    :draw draw
    :size [width height]
    ;:mouse-moved mouse-moved
    ;:mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-released key-released
    :key-pressed key-pressed))

