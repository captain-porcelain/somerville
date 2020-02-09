(ns somerville.visualization.gaia
  (:require
    [somerville.color.color :as color]
    [somerville.maps.gaia.core :as gaia]
    [talos.core :as talos]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def tau (* 2 Math/PI))

(def width 1200)
(def height 800)

(def colors
  {:background (color/rgba  10  10  10)
   :line       (color/rgba 217  60 110)
   :fill       (color/rgba  40  40  40 180)
   :focus-line (color/rgba 241 196  15)
   :focus-fill (color/rgba  80  80  80 128)})


;;====================================================================================================
;; Data Handling

(def fsm (reagent/atom nil))
(def worker (atom nil))

(def world (atom (gaia/icosahedron 400)))

(def draw-mode (reagent/atom :triangles))
(def index (reagent/atom 0))

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

(defn receive!
  [msg]
  (log/info msg))

(defn start-worker!
  "Test background worker process."
  []
  (let [w (js/Worker. "cljs-out/somerville-main-worker.js")
        ;tmp (do (.postMessage w "start"))
        tmp (set! (.-onmessage w) receive!)]
    (reset! worker w)))

;;====================================================================================================
;; Drawing Functionality

(defn draw-point
  "Draws one point of the world."
  [p lc]
  (quil/stroke (:r lc) (:g lc) (:b lc) (:a lc))
  (quil/point (:x p) (:y p) (:z p)))

(defn draw-line
  "Draws one line of the world."
  [l lc]
  (quil/stroke (:r lc) (:g lc) (:b lc) (:a lc))
  (quil/line (:x (:p1 l)) (:y (:p1 l)) (:z (:p1 l)) (:x (:p2 l)) (:y (:p2 l)) (:z (:p2 l))))

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
            (case @draw-mode
              :triangles (draw-triangle l (:fill colors) (:line colors))
              :lines (draw-line l (:line colors))
              :points (draw-point l (:line colors)))))
        (draw-triangle (nth @world @index) (:focus-fill colors) (:focus-line colors))))))


;;=====================================================================================================================
;; Define the FSM that handles gaia

(defn idle-callback
  [my-fsm event]
  (log/info "Calculation done..."))

(defn calculating-callback
  [my-fsm event]
  (log/info "Starting calculation..."))

(defn cycle-up-callback
  "Cycle through faces."
  []
  (when (< @index (dec (count @world))) (swap! index inc))
  (talos/process! @fsm {:event :done}))

(defn cycle-down-callback
  "Cycle through faces."
  []
  (when (> @index 0) (swap! index dec))
  (talos/process! @fsm {:event :done}))

(defn make-cube-callback
  "Reset world to cube."
  []
  (reset! index 0)
  (reset! draw-mode :triangles)
  (reset! world (gaia/cube 150))
  (talos/process! @fsm {:event :done}))

(defn make-icosahedron-callback
  "Reset world to icosahedron."
  []
  (reset! index 0)
  (reset! draw-mode :triangles)
  (reset! world (gaia/icosahedron 400))
  (talos/process! @fsm {:event :done}))

(defn make-delaunay-callback
  "Reset world to delaunay of fibonacci sphere."
  []
  (reset! index 0)
  (reset! draw-mode :triangles)
  (reset! world (gaia/delaunay 200))
  (talos/process! @fsm {:event :done}))

(defn make-voronoi-callback
  "Reset world to voronoi of fibonacci sphere."
  []
  (reset! index 0)
  (reset! draw-mode :lines)
  (reset! world (gaia/voronoi 100))
  (talos/process! @fsm {:event :done}))

(defn make-fibonacci-callback
  "Reset world to random fibonacci sphere."
  []
  (reset! index 0)
  (reset! draw-mode :points)
  (reset! world (gaia/fibonacci 200))
  (talos/process! @fsm {:event :done}))

(defn subdivide-callback
  "Subdivide current world."
  []
  (reset! index 0)
  (reset! world (gaia/subdivide @world))
  (talos/process! @fsm {:event :done}))

(def states
  (list (talos/state :idle             idle-callback)
        (talos/state :cycle-up         cycle-up-callback)
        (talos/state :cycle-down       cycle-down-callback)
        (talos/state :make-cube        make-cube-callback)
        (talos/state :make-delaunay    make-delaunay-callback)
        (talos/state :make-fibonacci   make-fibonacci-callback)
        (talos/state :make-icosahedron make-icosahedron-callback)
        (talos/state :make-voronoi     make-voronoi-callback)
        (talos/state :subdivide        subdivide-callback)
        (talos/state :calculating      calculating-callback)))

(def transitions
  (list (talos/transition :trigger-calculation     :idle                      :calculating)
        (talos/transition :key-pressed             :idle                      :cycle-up           (fn [event data] (= 171 (:data event)))) ;; +
        (talos/transition :key-pressed             :idle                      :cycle-down         (fn [event data] (= 173 (:data event)))) ;; -
        (talos/transition :key-pressed             :idle                      :make-cube          (fn [event data] (=  67 (:data event)))) ;; c
        (talos/transition :key-pressed             :idle                      :make-delaunay      (fn [event data] (=  68 (:data event)))) ;; d
        (talos/transition :key-pressed             :idle                      :make-fibonacci     (fn [event data] (=  70 (:data event)))) ;; f
        (talos/transition :key-pressed             :idle                      :make-icosahedron   (fn [event data] (=  73 (:data event)))) ;; i
        (talos/transition :key-pressed             :idle                      :subdivide          (fn [event data] (=  83 (:data event)))) ;; s
        (talos/transition :key-pressed             :idle                      :make-voronoi       (fn [event data] (=  86 (:data event)))) ;; v
        (talos/transition :done                    :*                         :idle)
        (talos/transition :calculation-finished    :calculating               :idle)))

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
    (reset! mouse-position [x y])))

(defn key-pressed []
  "Trigger actions on key presses."
  (talos/process! @fsm {:event :key-pressed :data (quil/key-code)}))


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
  (reset! fsm (talos/fsm states transitions))
  (start-worker!)
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
     [:li "f to recreate an fibonacci sphere"]
     [:li "d to recreate a delaunay of a fibonacci sphere"]
     [:li "v to recreate a voronoi of a fibonacci sphere"]
     [:li "s to subdivide"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "State: " (try (:name @(:state @fsm)) (catch js/Object e "")))]
     [:li (str "Surface Index: " @index)]
     [:li (str "Draw Mode: " @draw-mode)]]]])

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



