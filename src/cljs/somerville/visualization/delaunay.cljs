(ns somerville.visualization.delaunay
  (:require
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
(def w2 (/ width 2))
(def h2 (/ height 2))


(def colors
  {:background     (color/rgba  10  10  10)
   :point-voronoi  (color/rgba   0 204 102)
   :point-delaunay (color/rgba 247  92   3)
   :line-voronoi   (color/rgba 241 196  15)
   :line-delaunay  (color/rgba 217  60 110)})


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
  (reset! delaunay-triangles (delaunay/add-point @delaunay-triangles point))
  (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles)))

(defn make-fibonacci-sphere!
  "Create points for the projected fibonacci sphere."
  []
  (reset! fib-points (map proj/to-plane (sphere/fibonacci @fib-detail))))

(defn next-fibonacci-point!
  "Add next fibonacci point to diagram"
  []
  (when (< 0 (count @fib-points))
    (do
      (add-point! (first @fib-points))
      (swap! fib-points rest))))

(defn remaining-fibonacci-points!
  "Add remaining fibonacci points to diagram"
  []
  (dorun (map add-point! @fib-points))
  (reset! fib-points []))

(defn clear!
  "Clear the diagram."
  []
  (reset! points (list))
  (reset! delaunay-triangles (delaunay/delaunay @points (p/point (* -1 width) (* -1 height)) (p/point width height)))
  (reset! voronoi-lines (list)))

(defn reset-zoom!
  "Reset zoom."
  []
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

(defn bug!
  "Set points for debugging the missing line bug."
  []
  (let [p0 (p/point -296,4.4666595458984375)
        p1 (p/point -360,31.466659545898438)
        p2 (p/point -395,69.46665954589844)
        p3 (p/point -430,38.46665954589844)
        p4 (p/point -417,-50.53334045410156)
        p5 (p/point -382,-144.53334045410156)
        p6 (p/point -141,2.4666595458984375)
        p7 (p/point -301,278.46665954589844)
        p8 (p/point -296,60.46665954589844)
        p9 (p/point -295,-232.53334045410156)]
    (do
      (reset! points (list p0 p1 p2 p3 p4 p5 p6 p7 p8 p9))
      (reset! delaunay-triangles (delaunay/delaunay @points (p/point (* -1 width) (* -1 height)) (p/point width height)))
      (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles)))))

(defn debug!
  []
  (let [pa (p/point (quil/mouse-x) (quil/mouse-y))
        pr (p/point (- (quil/mouse-x) w2) (- (quil/mouse-y) h2))
        t (first (filter #(triangle/inside? (:t %) pr) (:triangles @delaunay-triangles)))
        ]
    (reset! info [(str "Absolute mouse: " (c/out pa))
                  (str "Relative mouse: " (c/out pr))
                  (str "Triangle: " (c/out t))])))


;;====================================================================================================
;; Drawing Functionality

(defn draw-point
  "Draw voronoi points."
  [point]
  (let [pv (:point-voronoi colors)
        p (p/scale point @zoom)]
    (do
      (quil/stroke (:r pv) (:g pv) (:b pv) (:a pv))
      (quil/fill (:r pv) (:g pv) (:b pv) (:a pv))
      (quil/point (+ (:x p) w2) (+ (:y p) h2)))))

(defn draw-line
  "Draw voronoi line."
  [line]
  (let [lv (:line-voronoi colors)
        l (line/scale line @zoom)]
    (do
      (quil/stroke (:r lv) (:g lv) (:b lv) (:a lv))
      (quil/fill (:r lv) (:g lv) (:b lv) (:a lv))
      (quil/line (+ (:x (:p1 l)) w2) (+ (:y (:p1 l)) h2) (+ (:x (:p2 l)) w2) (+ (:y (:p2 l)) h2)))))

(defn draw-triangle
  "Draw delaunay triangle"
  [t]
  (let [pd (:point-delaunay colors)
        c (p/scale (:p (:c t)) @zoom)]
    (do
      (quil/stroke (:r pd) (:g pd) (:b pd) (:a pd))
      (quil/fill (:r pd) (:g pd) (:b pd) (:a pd))
      (quil/point (+ (:x c) w2) (+ (:y c) h2))))
  (let [ld (:line-delaunay colors)
        t (triangle/scale (:t t) @zoom)]
    (do
      (quil/stroke (:r ld) (:g ld) (:b ld) (:a ld))
      (quil/fill (:r ld) (:g ld) (:b ld) (:a ld))
      (quil/line (+ (:x (:p1 t)) w2) (+ (:y (:p1 t)) h2) (+ (:x (:p2 t)) w2) (+ (:y (:p2 t)) h2))
      (quil/line (+ (:x (:p2 t)) w2) (+ (:y (:p2 t)) h2) (+ (:x (:p3 t)) w2) (+ (:y (:p3 t)) h2))
      (quil/line (+ (:x (:p3 t)) w2) (+ (:y (:p3 t)) h2) (+ (:x (:p1 t)) w2) (+ (:y (:p1 t)) h2)))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (let [bg (:background colors)]
    (quil/background (:r bg) (:g bg) (:b bg) (:a bg)))
  (dorun
    (for [p @points]
      (draw-point p)))
  (when @draw-delaunay
    (dorun
      (for [t (:triangles @delaunay-triangles)]
        ;(for [t (delaunay/remove-bounds @delaunay-triangles)]
        (draw-triangle t))))
  (when @draw-voronoi
    (dorun
      (for [l @voronoi-lines]
        (draw-line (:line l))))))


;;====================================================================================================
;; Event Handling

(defn mouse-released
  "Handle releasing mouse buttons."
  []
  (add-point! (p/point (- (quil/mouse-x) w2) (- (quil/mouse-y) h2))))

(defn mouse-wheel
  ""
  [direction]
  (zoom! direction))

(defn key-pressed []
  "Trigger actions on key presses."
  (case (quil/key-as-keyword)
    :b (bug!)
    :c (clear!)
    :f (make-fibonacci-sphere!)
    :n (next-fibonacci-point!)
    :a (remaining-fibonacci-points!)
    :+ (increase-fib!)
    :- (decrease-fib!)
    :p (print-points!)
    :i (debug!)
    :r (reset-zoom!)
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
     [:li "mouse wheel to zoom"]
     [:li "r to reset zoom"]
     [:li "c to clear diagram"]
     [:li "f to create projected fibonacci points"]
     [:li "a to add all projected fibonacci points"]
     [:li "n to add next projected fibonacci point"]
     [:li "+ to increase amount of fibonacci points"]
     [:li "- to decrease amount of fibonacci points"]
     [:li "b to create debugging diagram"]
     [:li "i to fetch debugging information"]
     [:li "p to print the points"]
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
             [:li (str "Count fibonacci points: " (count @fib-points))]
             [:li (str "Last point: " (if (nil? @last-point) "none" (c/out @last-point)))]
             [:li (str "Drawing delaunay: " @draw-delaunay)]
             [:li (str "Drawing voronoi: " @draw-voronoi)]
             [:li (str "Drawing zoom: " @zoom)]]
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
