(ns somerville.visualization.gaia
  (:require
    [somerville.color.color :as color]
    [somerville.visualization.commons :as commons]
    [talos.core :as talos]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def tau (* 2 Math/PI))

(def width 1200)
(def height 800)

(def colors
  {:background (color/rgba  10  10  10)
   :heavens    (color/rgba 129 178 154 128)
   :line       (color/rgba 217  60 110)
   :fill       (color/rgba  40  40  40 255)
   :point      (color/rgba   0 204 102)
   :focus-line (color/rgba 241 196  15)
   :focus-fill (color/rgba  80  80  80 128)})


;;====================================================================================================
;; Data Handling

(def fsm (reagent/atom nil))
(def worker (atom nil))

(def world (atom []))
(def fibonacci-size (reagent/atom 100))

(def draw-mode (reagent/atom :triangles))
(def index (reagent/atom 0))

(def mouse-position (atom [0 0]))
(def position (atom [61 -249]))

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

(defn draw-cell
  "Draws one voronoi cell representing an area of the world."
  [cell fc lc pc]
  (if (:closed cell)
    (quil/fill (:r fc) (:g fc) (:b fc) (:a fc))
    (quil/fill 255 255 255 0))
  (quil/stroke (:r lc) (:g lc) (:b lc) (:a lc))
  (quil/begin-shape)
  (dorun (map #(quil/vertex (:x %) (:y %) (:z %)) (:points cell)))
  (quil/end-shape)
  (draw-point (:point cell) pc))

(defn draw-heavens
  "Draw indicator for poles."
  []
  (let [c (:heavens colors)]
    (do
      (quil/stroke-weight 3)
      (quil/fill (:r c) (:g c) (:b c) (:a c))
      (quil/stroke (:r c) (:g c) (:b c) (:a c))
      (quil/line 0 0 300 0 0 -300)
      (quil/torus 300 5 128 128)
      (quil/with-translation [0 0 -300]
        (quil/with-rotation [(/ tau 4) 1 0 0]
          (quil/cone 6 25)))
      (quil/with-translation [0 0 300]
        (quil/with-rotation [(/ tau 4) 1 0 0]
          (quil/cone 6 25)))
      (quil/stroke-weight 1))))

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
              :cells (draw-cell l (:fill colors) (:line colors) (:point colors))
              :triangles (draw-triangle l (:fill colors) (:line colors))
              :lines (draw-line l (:line colors))
              :points (draw-point l (:point colors)))))
        (draw-heavens)
        (when (< 0 (count @world))
          (draw-triangle (nth @world @index) (:focus-fill colors) (:focus-line colors)))))))


;;=====================================================================================================================
;; Define the FSM that handles gaia

(defn idle-callback
  [my-fsm event]
  (log/info "Idle state reached."))

(defn cycle-up-callback
  "Cycle through faces."
  [my-fsm event]
  (when (< @index (dec (count @world))) (swap! index inc))
  (talos/process! @fsm {:event :done}))

(defn cycle-down-callback
  "Cycle through faces."
  [my-fsm event]
  (when (> @index 0) (swap! index dec))
  (talos/process! @fsm {:event :done}))

(defn increase-fibonacci-callback
  "Increase amount of fibonacci points for next generation."
  [my-fsm event]
  (swap! fibonacci-size #(+ % 10))
  (talos/process! @fsm {:event :done}))

(defn decrease-fibonacci-callback
  "Decrease amount of fibonacci points for next generation."
  [my-fsm event]
  (when (> @fibonacci-size 0) (swap! fibonacci-size #(- % 10)))
  (talos/process! @fsm {:event :done}))

(defn request-delaunay-callback
  "Reset world to delaunay of fibonacci sphere."
  [my-fsm event]
  (.postMessage @worker (commons/to-arraybuffer {:module "gaia"
                                                 :command "delaunay"
                                                 :data {:draw-mode :triangles
                                                        :scale 200
                                                        :points @fibonacci-size}}))
  (talos/process! @fsm {:event :work-requested}))

(defn request-voronoi-callback
  "Reset world to voronoi of fibonacci sphere."
  [my-fsm event]
  (.postMessage @worker (commons/to-arraybuffer {:module "gaia"
                                                 :command "voronoi"
                                                 :data {:draw-mode :cells
                                                        :scale 200
                                                        :points @fibonacci-size}}))
  (talos/process! @fsm {:event :work-requested}))

(defn request-fibonacci-callback
  "Reset world to random fibonacci sphere."
  [my-fsm event]
  (.postMessage @worker (commons/to-arraybuffer {:module "gaia"
                                                 :command "fibonacci"
                                                 :data {:draw-mode :points
                                                        :scale 200
                                                        :points @fibonacci-size}}))
  (talos/process! @fsm {:event :work-requested}))


(defn got-world-callback
  "Reset world to calculated data."
  [my-fsm event]
  (reset! index 0)
  (reset! draw-mode (keyword (:draw-mode (:data (:data event)))))
  (reset! world (:result (:data event)))
  (talos/process! @fsm {:event :done}))

(def states
  (list
    (talos/state :starting             idle-callback)
    (talos/state :idle                 idle-callback)
    (talos/state :cycle-up             cycle-up-callback)
    (talos/state :cycle-down           cycle-down-callback)
    (talos/state :increase-fib         increase-fibonacci-callback)
    (talos/state :decrease-fib         decrease-fibonacci-callback)
    (talos/state :request-delaunay     request-delaunay-callback)
    (talos/state :request-fibonacci    request-fibonacci-callback)
    (talos/state :request-voronoi      request-voronoi-callback)
    (talos/state :wait-world           idle-callback)
    (talos/state :got-world            got-world-callback)))

(def transitions
  (list
    (talos/transition :done-worker-start       :starting             :idle)
    (talos/transition :key-pressed             :idle                 :cycle-up            (fn [event data] (= 171 (:data event)))) ;; +
    (talos/transition :key-pressed             :idle                 :cycle-down          (fn [event data] (= 173 (:data event)))) ;; -
    (talos/transition :key-pressed             :idle                 :increase-fib        (fn [event data] (=  77 (:data event)))) ;; m
    (talos/transition :key-pressed             :idle                 :decrease-fib        (fn [event data] (=  76 (:data event)))) ;; l
    (talos/transition :key-pressed             :idle                 :request-delaunay    (fn [event data] (=  68 (:data event)))) ;; d
    (talos/transition :work-requested          :request-delaunay     :wait-world)
    (talos/transition :done-delaunay           :wait-world           :got-world)
    (talos/transition :key-pressed             :idle                 :request-fibonacci   (fn [event data] (=  70 (:data event)))) ;; f
    (talos/transition :work-requested          :request-fibonacci    :wait-world)
    (talos/transition :done-fibonacci          :wait-world           :got-world)
    (talos/transition :key-pressed             :idle                 :request-voronoi     (fn [event data] (=  86 (:data event)))) ;; v
    (talos/transition :work-requested          :request-voronoi      :wait-world)
    (talos/transition :done-voronoi            :wait-world           :got-world)
    (talos/transition :done                    :*                    :idle)))

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

(defn key-pressed
  "Trigger actions on key presses."
  []
  (talos/process! @fsm {:event :key-pressed :data (quil/key-code)}))

(defn receive!
  "Receive messages from webworker."
  [msg]
  (let [message (commons/to-clojurescript (.-data msg))
        tmp (log/info (str "Received work done message " (:command message)))]
    (talos/process! @fsm {:event (keyword (str "done-" (:command message)))
                          :data {:data (:data message)
                                 :result (:result message)}})))


;;====================================================================================================
;; App Setup

(defn start-worker!
  "Test background worker process."
  []
  (let [w (js/Worker. "cljs-out/somerville-main-worker.js")
        tmp (set! (.-onmessage w) receive!)]
    (reset! worker w)))

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
  (let [tmp-fsm (talos/fsm states transitions)
        state (reagent/atom @(:state tmp-fsm))]
    (reset! fsm (assoc tmp-fsm :state state)))
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
     [:li "f to recreate an fibonacci sphere"]
     [:li "d to recreate a delaunay of a fibonacci sphere"]
     [:li "v to recreate a voronoi of a fibonacci sphere"]
     [:li "m to increase points on fibonacci sphere"]
     [:li "l to decrease points on fibonacci sphere"]]]])

(defn settings
  "Show information current settings."
  [props]
  (let [state (:state @fsm)]
    [:div
     [:h3 "Settings"]
     [:span
      [:ul
       [:li (str "State: " (try (:name @state) (catch js/Object e (:state @fsm))))]
       [:li (str "Surface Count: " (count @world))]
       [:li (str "Surface Index: " @index)]
       [:li (str "Draw Mode: " @draw-mode)]
       [:li (str "Fibonacci Size: " @fibonacci-size)]]]]))

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

