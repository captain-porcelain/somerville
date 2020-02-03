(ns somerville.visualization.terrain
  (:require [somerville.geometry.commons :as c]
            [somerville.geometry.line :as line]
            [somerville.geometry.point :as p]
            [taoensso.timbre :as log]
            [quil.core :as quil :include-macros true]))

(def width 1200)
(def height 800)

(defn draw-line
  [l]
  (quil/stroke 22 255 50)
  (quil/fill 22 255 50)
  (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 222 222 128)
  (quil/no-fill)
  (quil/rect 0 0 width height)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  )

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn mouse-pressed [])
(defn mouse-released []
  (let [mx (quil/mouse-x)
        my (quil/mouse-y)
        pm (p/point mx my)]
    ))

(defn key-pressed []
  "Trigger actions on key presses."
    ;(dorun (println (str "pressed code " (quil/key-code))))
    (case (quil/key-as-keyword)
      :c (log/info "clear")
      :t (log/info "create terrain")
      (dorun (println (str "pressed key " (quil/key-as-keyword))))))

(defn ^:export show []
  (quil/defsketch terrain
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

