(ns terrain.core)
;; ClojureScript port of Realistic terrain in 130 lines
;; original source http://www.playfuljs.com/realistic-terrain-in-130-lines/

(defn translate-coord [size x y]
  (int (+ x (* y size))))

(defn get-value [{:keys [values size]} x y]
  (aget values (translate-coord size x y)))

(defn set-value [{:keys [values size]} x y value]
  (aset values (translate-coord size x y) value))

(defn average [values]
  (/ (reduce + values) (count values)))

(defn in-range [max [x y]]
  (and (< -1 x max) (< -1 y max)))

(defn generate-value [terrain offset coords]
  (->> coords
       (filter #(in-range (:max terrain) %))
       (map #(apply get-value terrain %))
       (average)
       (+ offset)))

(defn square [terrain tile-size offset x y]
  (->> [[(- x tile-size) (- y tile-size)]
        [(+ x tile-size) (- y tile-size)]
        [(+ x tile-size) (+ y tile-size)]
        [(- x tile-size) (+ y tile-size)]]
       (generate-value terrain offset)
       (set-value terrain x y)))

(defn diamond [terrain tile-size offset x y]
  (->> [[x (- y tile-size)]
        [(+ x tile-size) y]
        [x (+ y tile-size)]
        [(- x tile-size) y]]
       (generate-value terrain offset)
       (set-value terrain x y)))

(defn random-offset [scale]
  (- (* (js/Math.random) scale 2) scale))

(defn divide-squares [terrain tile-size half scale]
  (loop [y half]
    (when (< y (:max terrain))
      (loop [x half]
        (when (< x (:max terrain))
          (square terrain half (random-offset scale) x y)
          (recur (+ x tile-size))))
      (recur (+ y tile-size)))))

(defn divide-diamonds [terrain tile-size half scale]
  (loop [y 0]
    (when (< y (:max terrain))
      (loop [x (mod (+ y half) tile-size)]
        (when (< x (:max terrain))
          (diamond terrain half (random-offset scale) x y)
          (recur (+ x tile-size))))
      (recur (+ y half)))))

(defn divide-tiles [terrain roughness tile-size]
  (let [half  (/ tile-size 2)
        scale (* roughness tile-size)]
    (when-not (< half 1)
      (divide-squares terrain tile-size half scale)
      (divide-diamonds terrain tile-size half scale)
      (recur terrain roughness half))))

(defn generate [detail roughness]
  (let [size    (inc (js/Math.pow 2 detail))
        max     (dec size)
        values  (js/Float32Array. (* size size))
        terrain {:size   size
                 :max    max
                 :values values}]
    (set-value terrain 0 0 max)
    (set-value terrain max 0 (/ max 2))
    (set-value terrain max max 0)
    (set-value terrain 0 max (/ max 2))
    (divide-tiles terrain roughness max)
    terrain))

(defn iso [size x y]
  {:x (* 0.5 (- (+ size x) y))
   :y (* 0.5 (+ x y))})

(defn project [size width height x y z]
  (let [point (iso size x y)
        x0    (* width 0.5)
        y0    (* height 0.2)
        x     (* 15 (- (:x point) (* size 0.5)))
        y     (* 3 (inc (* 0.005 (- size (:y point)))))
        z     (- (* size 3) (+ (* 0.4 z) (* (:y point) 0.05)))]
    {:x (+ x0 (/ x y))
     :y (+ y0 (/ z y))}))

(defn brightness [max x y slope]
  (let [b (+ 128 (js/Math.floor (* slope 50)))]
    (cond
      (< b 128)
      (str "rgba(" b "," (* 2 b) "," b ",1)")
      (<= 128 b 220)
      (str "rgba(" b "," b "," (/ b 3) ",1)")
      :else
      (str "rgba(" b "," b "," b ",1)"))))

(defn rect [ctx a b style]
  (when (> (:y b) (:y a))
    (set! (.-fillStyle ctx) style)
    (.fillRect ctx (:x a) (:y a) (- (:x b) (:x a)) (- (:y b) (:y a)))))

(defn draw [ctx {:keys [size] :as terrain} width height]
  (let [water-val (* size 0.7)
        project   (partial project size width height)]
    (dotimes [y size]
      (dotimes [x size]
        (let [val    (get-value terrain x y)
              top    (project x y val)
              bottom (project (inc x) y 0)
              water  (project x y water-val)
              style  (brightness max x y (- (get-value terrain (inc x) y) val))]
          (rect ctx top bottom style)
          (rect ctx water bottom "rgba(50, 150, 200, 0.15)"))))))

(defn clear [ctx width height]
  (set! (.-fillStyle ctx) "black")
  (.fillRect ctx 0 0 width height))

(defn init! []
  (let [canvas (js/document.getElementById "canvas")
        ctx     (.getContext canvas "2d")
        width   js/window.innerWidth
        height  js/window.innerHeight
        terrain (generate 9 1)]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (clear ctx width height)
    (draw ctx terrain width height)))

(init!)
