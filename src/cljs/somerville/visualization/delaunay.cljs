(ns somerville.visualization.delaunay
  (:require
    [somerville.commons :as sc]
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as p]
    [somerville.geometry.sphere :as sphere]
    [somerville.geometry.delaunay :as delaunay]
    [somerville.geometry.projection.stereographic :as proj]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]
    [somerville.geometry.triangle :as triangle]))

(def width 1200)
(def height 800)
(def mouse-position (atom [0 0]))
(def position (atom [0 0]))
(def offset-width (atom (/ width 2)))
(def offset-height (atom (/ height 2)))


(def colors
  {:background      (color/rgba  40  40  40)

   :point-voronoi   (color/rgba 158   0  83)
   :line-voronoi    (color/rgba 255   0 128)

   :point-delaunay  (color/rgba 254  78   0)
   :line-delaunay   (color/rgba 230 127  13)

   :point-invalid   (color/rgba  18  53  91)
   :line-invalid    (color/rgba  27 153 139)

   :line-highlight  (color/rgba 255 255 255)
   :point-highlight (color/rgba 255 255 255)

   :point-next      (color/rgba  57   0 153)})


;;====================================================================================================
;; Data Handling

(def points (reagent/atom (list)))
(def delaunay-triangles (atom (delaunay/delaunay @points (p/point (* -1 width) (* -1 height)) (p/point width height))))
(def voronoi-lines (atom (delaunay/voronoi @delaunay-triangles)))
(def draw-delaunay (reagent/atom true))
(def draw-voronoi (reagent/atom true))
(def last-point (reagent/atom nil))
(def info (reagent/atom []))
(def zoom (reagent/atom 1))
(def fib-detail (reagent/atom 10))
(def fib-points (reagent/atom []))
(def invalidated (reagent/atom []))
(def highlight-triangle (reagent/atom nil))

(defn increase-fib!
  "Increase amount of fibonacci points"
  []
  (swap! fib-detail inc))

(defn decrease-fib!
  "Decrease amount of fibonacci points"
  []
  (swap! fib-detail #(if (< 2 %) (dec %) %)))

(defn add-point!
  "Add another point to the diagram."
  [point]
  (reset! points (cons point @points))
  (reset! last-point point)
  (reset! invalidated [])
  (reset! highlight-triangle nil)
  (reset! delaunay-triangles (delaunay/add-point @delaunay-triangles point))
  (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles)))

(defn unadd-point!
  "Remove last point"
  []
  (reset! points (rest @points))
  (reset! invalidated [])
  (reset! highlight-triangle nil)
  (reset! delaunay-triangles (delaunay/delaunay @points (p/point (* -1 width) (* -1 height)) (p/point width height)))
  (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles)))

(defn next-triangle!
  "Cycle through triangles."
  []
  (swap! highlight-triangle
         #(let [c (count (:triangles @delaunay-triangles))]
            (cond
              (= % (dec c)) nil
              (and (nil? %) (< 0 c)) 0
              (< 0 c) (inc %)))))

(defn make-fibonacci-sphere!
  "Create points for the projected fibonacci sphere."
  []
  (reset! invalidated [])
  (reset! highlight-triangle nil)
  (reset! fib-points (map #(p/scale % 10) (map proj/to-plane (sphere/fibonacci @fib-detail)))))

(defn next-fibonacci-point!
  "Add next fibonacci point to diagram"
  []
  (reset! invalidated [])
  (reset! highlight-triangle nil)
  (when (< 0 (count @fib-points))
    (do
      (add-point! (first @fib-points))
      (swap! fib-points rest))))

(defn remaining-fibonacci-points!
  "Add remaining fibonacci points to diagram"
  []
  (reset! invalidated [])
  (reset! highlight-triangle nil)
  (dorun (map add-point! @fib-points))
  (reset! fib-points []))

(defn highlight-invalidated!
  "Highlight the triangles that are invalidated by the next fibonacci point."
  []
  (let [p (first @fib-points)
        classified (map (fn [t] [(delaunay/invalidates? t p) t]) (:triangles @delaunay-triangles))
        nok (map second (filter first classified))]
    (reset! invalidated nok)))

(defn clear!
  "Clear the diagram."
  []
  (reset! fib-points [])
  (reset! invalidated [])
  (reset! highlight-triangle nil)
  (reset! points (list))
  (reset! delaunay-triangles (delaunay/delaunay @points (p/point (* -1 width) (* -1 height)) (p/point width height)))
  (reset! voronoi-lines (list)))

(defn reset-zoom!
  "Reset zoom."
  []
  (reset! offset-width (/ width 2))
  (reset! offset-height (/ height 2))
  (reset! zoom 1))

(defn zoom!
  "Zoom the points"
  [direction]
  (reset! zoom (if (pos? direction) (* 0.9 @zoom) (* 1.1 @zoom))))

(defn toggle-drawing-delaunay!
  "Toggle drawing the delaunay triangles."
  []
  (reset! draw-delaunay (not @draw-delaunay)))

(defn toggle-drawing-voronoi!
  "Toggle drawing the voronoi diagram."
  []
  (reset! draw-voronoi (not @draw-voronoi)))

(defn print-points!
  "Print the points of the diagram."
  []
  (dorun (map #(log/info (c/out %)) @points)))

(defn debug!
  []
  (let [pa (p/point (quil/mouse-x) (quil/mouse-y))
        pr (p/point (- (quil/mouse-x) @offset-width) (- (quil/mouse-y) @offset-height))
        t (first (filter #(triangle/inside? (:t %) pr) (:triangles @delaunay-triangles)))]
    (reset! info [(str "Absolute mouse: " (c/out pa))
                  (str "Relative mouse: " (c/out pr))
                  (str "Triangle: " (c/out t))])))


;;====================================================================================================
;; Drawing Functionality

(defn draw-point
  "Draw a point."
  [point col]
  (let [p (p/scale point @zoom)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/stroke-weight 5)
      (quil/point (+ (:x p) @offset-width) (+ (:y p) @offset-height))
      (quil/stroke-weight 1))))

(defn draw-circle
  "Draw a cirlce."
  [c col]
  (let [p (p/scale (:p c) @zoom)
        d (* (:r c) @zoom 2)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) 40)
      (quil/ellipse (+ (:x p) @offset-width) (+ (:y p) @offset-height) d d))))

(defn draw-line
  "Draw a line."
  [line col]
  (let [l (line/scale line @zoom)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/line (+ (:x (:p1 l)) @offset-width) (+ (:y (:p1 l)) @offset-height) (+ (:x (:p2 l)) @offset-width) (+ (:y (:p2 l)) @offset-height)))))

(defn draw-triangle
  "Draw a triangle"
  [t pcol lcol]
  (draw-point (:p (:c t)) pcol)
  (draw-line (line/line (:p1 (:t t)) (:p2 (:t t))) lcol)
  (draw-line (line/line (:p2 (:t t)) (:p3 (:t t))) lcol)
  (draw-line (line/line (:p3 (:t t)) (:p1 (:t t))) lcol))

(defn draw-invalidated?
  "Check if the invalidated should be drawn."
  []
  (and
    @draw-delaunay
    (not (nil? (first @fib-points)))
    (< 0 (count @invalidated))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background (:r (:background colors)) (:g (:background colors)) (:b (:background colors)) (:a (:background colors)))

  (dorun
    (map #(draw-point % (:point-voronoi colors)) @points))

  (when @draw-delaunay
    (dorun
      (map #(draw-triangle % (:point-delaunay colors) (:line-delaunay colors)) (:triangles @delaunay-triangles))))

  (when (draw-invalidated?)
    (do
      (draw-point (first @fib-points) (:next-point colors))
      (dorun
        (map #(draw-triangle % (:line-invalid colors) (:point-invalid colors)) @invalidated))))

  (when @draw-voronoi
    (dorun
      (map #(draw-line (:line %) (:line-voronoi colors)) @voronoi-lines)))

  (when-not (nil? @highlight-triangle)
    (do
      (draw-circle (:c (nth (:triangles @delaunay-triangles) @highlight-triangle)) (:line-highlight colors))
      (draw-triangle (nth (:triangles @delaunay-triangles) @highlight-triangle) (:line-highlight colors) (:point-highlight colors)))))


;;====================================================================================================
;; Event Handling

(defn mouse-dragged
  "calculate position change and store in atom"
  []
  (case (quil/mouse-button)
    :left (let [x (quil/mouse-x)
                y (quil/mouse-y)
                [old-x old-y] @mouse-position
                p-x @offset-width
                p-y @offset-height]
            (reset! mouse-position [x y])
            (reset! offset-width (+ p-x (- x old-x)))
            (reset! offset-height (+ p-y (- y old-y))))
    nil))

(defn mouse-pressed
  "store mouse position in atom"
  []
  (case (quil/mouse-button)
    :left (let [x (quil/mouse-x)
                y (quil/mouse-y)]
            (reset! mouse-position [x y]))
    nil))

(defn mouse-released
  "Handle releasing mouse buttons."
  []
  (case (quil/mouse-button)
    :right (add-point!
             (p/scale
               (p/point (- (quil/mouse-x) @offset-width) (- (quil/mouse-y) @offset-height))
               (/ 1 @zoom)))
    nil))

(defn mouse-wheel
  "Handle mouse wheel events."
  [direction]
  (zoom! direction))

(defn key-pressed []
  "Trigger actions on key presses."
  (case (quil/key-as-keyword)
    :c (clear!)
    :u (unadd-point!)
    :f (make-fibonacci-sphere!)
    :n (next-fibonacci-point!)
    :a (remaining-fibonacci-points!)
    :+ (increase-fib!)
    :- (decrease-fib!)
    :p (print-points!)
    :i (debug!)
    :h (highlight-invalidated!)
    :r (reset-zoom!)
    :t (next-triangle!)
    :d (toggle-drawing-delaunay!)
    :v (toggle-drawing-voronoi!)
    (log/info (str "pressed key " (quil/key-as-keyword)))))


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
  (quil/defsketch delaunay-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-wheel mouse-wheel
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Voronoi by Delaunay"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "left mouse button to add point"]
     [:li "u to remove last point"]
     [:li "mouse wheel to zoom"]
     [:li "r to reset zoom"]
     [:li "c to clear diagram"]
     [:li "f to create projected fibonacci points"]
     [:li "+ to increase amount of fibonacci points"]
     [:li "- to decrease amount of fibonacci points"]
     [:li "a to add all projected fibonacci points"]
     [:li "n to add next projected fibonacci point"]
     [:li "h to highlight triangles invalidated by next fibonacci point"]
     [:li "i to fetch debugging information"]
     [:li "p to print the points"]
     [:li "t to cycle through the triangles"]
     [:li "d to toggle drawing delaunay"]
     [:li "v to toggle drawing voronoi"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    (into []
          (concat
            [:ul
             [:li (str "Count points: " (count @points))]
             [:li (str "fibonacci details: " @fib-detail)]
             [:li (str "invalidated points: " (count @invalidated))]
             [:li (str "Count fibonacci points: " (count @fib-points))]
             [:li (str "Last point: " (if (nil? @last-point) "none" (c/out @last-point)))]
             [:li (str "Drawing delaunay: " @draw-delaunay)]
             [:li (str "Drawing voronoi: " @draw-voronoi)]
             [:li (str "Drawing zoom: " @zoom)]
             [:li (str "Highlighted: " @highlight-triangle)]]
            (map (fn [i] [:li i]) @info)))]])

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
