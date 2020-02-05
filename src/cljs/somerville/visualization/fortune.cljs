(ns somerville.visualization.fortune
  (:require
    [somerville.geometry.line :as line]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.parabola :as parabola]
    [somerville.geometry.fortune :as fortune]
    [somerville.geometry.voronoi :as voronoi]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def colors
  {:background  (color/rgba  10  10  10)
   :line-low    (color/rgba 241 196  15)
   :line-high   (color/rgba 217  60 110)})


;;====================================================================================================
;; Data Handling

(def p1 (p/point 200 250))
(def p2 (p/point 500 450))
(def p3 (p/point 100 500))
(def p4 (p/point 250 50))
(def points (reagent/atom (list p1 p2 p3 p4)))
(def sites (atom (fortune/voronoi @points)))
(def vsites (atom (voronoi/voronoi @points 0 0 width height)))

(defn add-point!
  "Add another point to the diagram."
  [point]
  (reset! points (cons point @points))
  (reset! vsites (voronoi/voronoi @points 0 0 width height))
  (reset! sites (fortune/voronoi @points)))

(defn clear!
  "Clear the diagram."
  []
  (reset! points (list))
  (reset! vsites (voronoi/voronoi @points 0 0 width height))
  (reset! sites (fortune/voronoi @points)))

(defn debug!
  "Print debugging informantion"
  []
  (log/info (c/out @sites)))

(defn step!
  "Make a step in fortunes algorithm"
  []
  (reset! sites (fortune/step @sites)))

(defn reset-steps!
  "Reset fortunes algorithm."
  []
  (reset! vsites (voronoi/voronoi @points 0 0 width height))
  (reset! sites (fortune/voronoi @points)))

;;====================================================================================================
;; Drawing Functionality

(defn draw-intersection
  [i]
  (quil/stroke 0 0 255)
  (quil/fill 0 0 255)
  (quil/rect (:x (:intersection i)) (:y (:intersection i)) 4 4))

(defn draw-bisector
  [bisector]
  (let [y0 (line/solve-line-at (:line bisector) 0)
        y1 (line/solve-line-at (:line bisector) width)]
    (quil/stroke 0 255 0)
    (quil/fill 0 255 0)
    (quil/line 0 y0 width y1)))

(defn draw-parabola
  [parabola]
  (let [xs (range (quil/width))]
    (quil/stroke 237 237 177)
    (quil/fill 237 237 177)
    (dorun
      (for [x xs]
        (quil/line x (parabola/solve-parabola-at parabola x) (+ x 1) (parabola/solve-parabola-at parabola (+ x 1)))))))

(defn draw-sweepline
  [y]
  (quil/stroke 211 248 226)
  (quil/fill 211 248 226)
  (quil/line 0 y width y))

(defn draw-site
  [site]
  (quil/stroke 246 148 193)
  (quil/fill 246 148 193)
  (quil/rect (- (:x site) 2) (- (:y site) 2) 4 4))

(defn draw-event
  [site]
  (quil/stroke 211 248 226)
  (quil/fill 211 248 226)
  (quil/rect (- (:x (:point site)) 2) (- (:y (:point site)) 2) 4 4))

(defn draw-edge
  [edge]
  (quil/stroke 169 222 249)
  (quil/fill 169 222 249)
  (when (and (not (nil? (:left edge))) (not (nil? (:right edge))))
    (let [m (p/midpoint (:left edge) (:right edge))]
      (quil/line (:x (:start edge)) (:y (:start edge)) (:x m) (:y m)))))

(defn draw-cell
  [cell]
  (quil/stroke 200 200 200)
  (quil/fill 200 200 200)
  (dorun (map #(quil/line (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) (:lines cell))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (let [bg (:background colors)]
    (quil/background (:r bg) (:g bg) (:b bg) (:a bg)))
  (dorun
	(for [site (:cells @vsites)]
	  (draw-cell site)))
  (dorun
    (for [site (:points @sites)]
      (draw-site site)))
  (dorun
    (for [site (:events @sites)]
      (draw-event site)))
  (dorun
    (for [node (fortune/sequence-from-tree (:tree @sites))]
      (when (not (nil? (:edge node))) (draw-edge (:edge node)))))
  (let [step (- (:step @sites) 1)
        sweep-y (if (and (>= step 0) (< step (count (:points @sites))))
                  (:y (nth (:points @sites) step))
                  nil)]
    (if (not (nil? sweep-y))
      (do
        (dorun
          (for [site (take (:step @sites) (:points @sites))]
            (draw-parabola (parabola/parabola-from-focuspoint-and-directrix-y site (+ 1 sweep-y)))))
        (draw-sweepline sweep-y)))))


;;====================================================================================================
;; Event Handling

(defn mouse-released
  "Handle releasing mouse buttons."
  []
  (add-point! (p/point (quil/mouse-x) (quil/mouse-y))))

(defn key-pressed []
  "Trigger actions on key presses."
  (case (quil/key-code)
    32 (step!)  ;; space
    67 (clear!) ;; c
    68 (debug!) ;; d
    82 (reset-steps!) ;; r
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
  (quil/defsketch fortune-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-released mouse-released
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Voronoi by Fortune"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "left mouse button to add point"]
     [:li "space for progressing fortune"]
     [:li "c to clear diagram"]
     [:li "d to print debugging info"]
     [:li "r to reset processing steps"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Count points: " (count @points))]]]])

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


