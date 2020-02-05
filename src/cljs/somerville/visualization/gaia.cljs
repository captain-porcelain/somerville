(ns somerville.visualization.gaia
  (:require
    [somerville.color.color :as color]
    [somerville.geometry.polygon :as polygon]
    [somerville.maps.gaia.core :as gaia]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def tau (* 2 Math/PI))

(def width 1200)
(def height 800)

(def colors
  {:background (color/rgba  10  10  10)
   :line       (color/rgba 217  60 110)
   :fill       (color/rgba  40  40  40 128)
   :focus-line (color/rgba 241 196  15)
   :focus-fill (color/rgba  80  80  80 128)})



;;====================================================================================================
;; Data Handling

(def index (reagent/atom 0))
(def world (atom (gaia/icosahedron 400)))
(def mouse-position (atom [0 0]))
(def position (atom [0 0]))

(defn get-mouse-angle-x
  "get mouse based angle"
  [width]
  (let [[x y] @position]
    (* tau (/ (- x width) width))))

(defn get-mouse-angle-y
  "get mouse based angle"
  [height]
  (let [[x y] @position]
    (* tau (/ (- height y) height))))

(defn cycle-up!
  "Cycle through faces."
  []
  (when (< @index (dec (count @world))) (swap! index inc)))

(defn cycle-down!
  "Cycle through faces."
  []
  (when (> @index 0) (swap! index dec)))

(defn regenerate-cube!
  "Reset world to cube."
  []
 (do
   (reset! index 0)
   (reset! world (gaia/cube 200))))

(defn regenerate-icosahedron!
  "Reset world to icosahedron."
  []
  (do
    (reset! index 0)
    (reset! world (gaia/icosahedron 400))))

(defn subdivide!
  "Subdivide current world."
  []
  (do
    (reset! index 0)
    (reset! world (gaia/subdivide @world))))


;;====================================================================================================
;; Drawing Functionality

(defn draw-triangle
  "Draws one polygon representing an area of the world."
  [t fc lc]
  (quil/fill (:r fc) (:g fc) (:b fc) (:a fc))
  (quil/stroke (:r lc) (:g lc) (:b lc) (:a lc))
  (quil/begin-shape :triangles)
  (quil/vertex (:x (:p1 t)) (:y (:p1 t)) (:z (:p1 t)))
  (quil/vertex (:x (:p2 t)) (:y (:p2 t)) (:z (:p2 t)))
  (quil/vertex (:x (:p3 t)) (:y (:p3 t)) (:z (:p3 t)))
  (quil/end-shape))

(defn draw
  "This function is called by processing repeatedly."
  []
  (let [bg (:background colors)]
    (quil/background (:r bg) (:g bg) (:b bg) (:a bg)))
  (quil/with-rotation [(get-mouse-angle-y width) 1 0 0]
    (quil/with-rotation [(get-mouse-angle-x height) 0 1 0]
      (quil/with-rotation [(/ 0 tau) 0 0 1]
        (dorun
          (for [l @world]
            (draw-triangle l (:fill colors) (:line colors))))
        (draw-triangle (nth @world @index) (:focus-fill colors) (:focus-line colors))))))


;;====================================================================================================
;; Event Handling

(defn mouse-dragged
  "calculate position change and store in atom"
  []
  (let [x (quil/mouse-x)
        y (quil/mouse-y)
        [old-x old-y] @mouse-position
        [p-x p-y] @position]
    (reset! mouse-position [x y])
    (reset! position [(+ p-x (- x old-x)) (+ p-y (- y old-y))])))

(defn mouse-pressed
  "store mouse position in atom"
  []
  (let [x (quil/mouse-x)
        y (quil/mouse-y)]
    ;(dorun (println (str "mouse pressed at " x " - " y)))
    (reset! mouse-position [x y])))

(defn key-pressed []
  "Trigger actions on key presses."
  (case (quil/key-code)
    171 (cycle-up!) ;; +
    173 (cycle-down!) ;; -
     67 (regenerate-cube!);; c
     73 (regenerate-icosahedron!) ;; i
     83 (subdivide!) ;; s
    (log/info "Pressed unhandled key with code" (quil/key-code))))


;;====================================================================================================
;; App Setup

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (let [bg (:background colors)]
    (quil/fill (:r bg) (:g bg) (:b bg) (:a bg)))
  (quil/frame-rate 10))

(defn init
  "Initialize Quil sketch."
  [canvas-id]
  (quil/defsketch gaia-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :renderer :p3d
    :mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Gaia Visualization"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "+ to cycle up through the surfaces"]
     [:li "- to cycle down through the surfaces"]
     [:li "c to recreate a cube"]
     [:li "i to recreate an icosahedron"]
     [:li "s to subdivide"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Surface Index:" @index)]]]])

(defn ui
  "Draw the basic ui for this visualization."
  [props]
  [:div {:class "row"}
   [:div {:id "hostelement" :class "column left" :on-load init}]
   [:div {:class "column right"}
    [usage]
    [settings]]])

(defn visualize
  "Render html and canvas for terrain visualization."
  [props]
  (reagent/create-class
    {:reagent-render ui
     :component-did-mount #(init "hostelement")}))



