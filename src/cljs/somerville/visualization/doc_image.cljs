(ns somerville.visualization.doc-image
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as line]
    [somerville.geometry.circle :as circle]
    [somerville.geometry.arc :as arc]
    [somerville.color.color :as color]
    [somerville.parsers.doc-image-parser :as parser]
    [quil.core :as quil :include-macros true]
    [taoensso.timbre :as log]
    [reagent.core :as reagent]))

(def width 600)
(def height 400)

(def offset-width (atom (/ width 2)))
(def offset-height (atom (/ height 2)))

(def colors
  {:background      (color/rgba  40  40  40)
   :ui              (color/rgba 200 200 200)})

;;====================================================================================================
;; Data Handling

;; A flag to indicate that the user wants to download the rendered image.
(def save? (atom false))

(defn request-save
  "Set the flag to trigger saving the image."
  []
  (reset! save? true))

;; The atom to hold the parsed rendering definitions.
(def definitions (atom []))

;; Hold the result of parsing the definitions.
(def error (reagent/atom "No Error"))

;; The default text to display after loading the visualization.
(def default-text
  "{
 :name \"point-angle\"
 :definitions
 {
  :p1 [:point  50  50]
  :p2 [:point 150 100]
  :p3 [:point 100 150]
  :l1 [:line  :p1 :p2]
  :l2 [:line  :p1 :p3]
  :a  [:arc-lines [:circle :p1 50] :l1 :l2]
 }
 :colors {
  :point  [222  60  75]
  :line   [135 245 251]
  :circle [166  99 204]
 }
 :renderings
 [
  {:name \"\"   :type :line  :object :l1 :text-offset [ 10  10] :color :line}
  {:name \"\"   :type :line  :object :l2 :text-offset [ 10  10] :color :line}
  {:name \"p1\" :type :point :object :p1 :text-offset [-10 -10] :color :point}
  {:name \"p2\" :type :point :object :p2 :text-offset [ 10 -10] :color :point}
  {:name \"p3\" :type :point :object :p3 :text-offset [ 10  10] :color :point}
  {:name \"a\"  :type :arc   :object :a  :text-offset [ 45  60] :color :circle}
 ]
}")

(defn update-definitions
  "Update the definitions from text area."
  []
  (try
    (let [nd (parser/parse (.-value (.getElementById js/document "desc-input")))
          tmp (dorun nd)
          tmp (reset! definitions nd)]
      (reset! error "No error"))
    (catch js/Object e
      (do
        (log/error e)
        (reset! error (str e))))))

;;====================================================================================================
;; Drawing Functionality

(defn draw-axis
  "Draw the axis"
  []
  (let [col (:ui colors)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/line 0 @offset-height width @offset-height)
      (quil/line @offset-width 0 @offset-width height)
      (quil/line width @offset-height (- width 10) (- @offset-height 10))
      (quil/line width @offset-height (- width 10) (+ @offset-height 10))
      (quil/line @offset-width height (- @offset-width 10) (- height 10))
      (quil/line @offset-width height (+ @offset-width 10) (- height 10))
      )))

(defn draw-point
  "Draw a point."
  [object]
  (let [point (:object object)
        col (:color object)
        tcol (:ui colors)
        x (+ (:x point) @offset-width)
        y (+ (:y point) @offset-height)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/stroke-weight 6)
      (quil/point x y)
      (quil/stroke-weight 1)
      (quil/stroke (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/fill (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/text (:name object) (+ x (first (:text-offset object))) (+ y (second (:text-offset object)))))))

(defn draw-line
  "Draw a line."
  [object]
  (let [line (:object object)
        col (:color object)
        tcol (:ui colors)
        tp (p/midpoint (:p1 line) (:p2 line))
        x (+ (:x tp) @offset-width)
        y (+ (:y tp) @offset-height)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/stroke-weight 2)
      (quil/line (+ (:x (:p1 line)) @offset-width) (+ (:y (:p1 line)) @offset-height) (+ (:x (:p2 line)) @offset-width) (+ (:y (:p2 line)) @offset-height))
      (quil/stroke-weight 1)
      (quil/stroke (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/fill (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/text (:name object) (+ x (first (:text-offset object))) (+ y (second (:text-offset object)))))))

(defn draw-circle
  "Draw an arc."
  [object]
  (let [c (:object object)
        cp (:p c)
        r (:r c)
        col (:color object)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill 255 255 255 0)
      (quil/stroke-weight 2)
      (quil/ellipse (+ (:x cp) @offset-width) (+ (:y cp) @offset-height) (* 2 r) (* 2 r))
      (quil/stroke-weight 1))))

(defn draw-arc
  "Draw an arc."
  [object]
  (let [arc (:object object)
        cp (:p (:c arc))
        r (:r (:c arc))
        col (:color object)
        tcol (:ui colors)
        a1 (p/angle cp (p/point (inc (:x cp)) (:y cp)) (:p1 arc))
        a2 (p/angle cp (p/point (inc (:x cp)) (:y cp)) (:p2 arc))
        x (+ (:x cp) @offset-width)
        y (+ (:y cp) @offset-height)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill 255 255 255 0)
      (quil/stroke-weight 2)
      (quil/arc x y (* 2 r) (* 2 r) a1 a2 :open)
      (quil/stroke-weight 1)
      (quil/stroke (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/fill (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/text (:name object) (+ x (first (:text-offset object))) (+ y (second (:text-offset object)))))))

(defn draw-object
  "Dispatch drawing objects by their type."
  [o]
  (case (:type o)
    :point (draw-point o)
    :line  (draw-line o)
    :circle (draw-circle o)
    :arc (draw-arc o)
    nil))

(defn handle-save
  "Handle a request to save the image."
  []
  (when @save?
    (do
      (quil/save (str (:name @definitions) ".png"))
      (reset! save? false))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (do (draw-axis))
  (dorun (map draw-object (:renderings @definitions)))
  (handle-save))


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
  (quil/defsketch lsystem-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Documentation Rendering"]
   [:h3 "Usage"]
   [:div
    [:ul
    [:li "Enter definitions of geometric objects in the textbox below and press"]
    [:li "'Render Image' to show the results of rendering your inputs"]
    [:li "'Save Image' to download the image"]
    [:li "See 'Parse Status' below for errors in your input"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:div "Parse Status:"]
   [:div @error]])

(defn ui
  "Draw the basic ui for this visualization."
  [props]
  [:div
   [:div {:class "row"}
    [:div {:id "hostelement" :class "column left" :on-load init}]
    [:div {:class "column right"}
     [usage]
     [settings]]]
   [:div {:class "row"}
    [:div {:class "column left"}
     [:textarea {:id "desc-input" :default-value default-text}]]
    [:div {:class "column right"}
     [:div {:class "button" :on-click update-definitions} "Render Image"]
     [:div {:class "button" :on-click request-save} "Save Image"]]
    ]])

(defn visualize
  "Render html and canvas for terrain visualization."
  [props]
  (reagent/create-class
    {:reagent-render ui
     :component-did-mount #(init "hostelement")}))


