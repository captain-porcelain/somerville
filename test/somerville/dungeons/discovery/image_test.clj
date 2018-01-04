;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.image-test
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon BasicStroke RenderingHints]
    [java.awt.image BufferedImage])
  (:require
    [somerville.image :as images]
    [somerville.dungeons.discovery.ray-cast-wall-trace :as rcwt]
    [somerville.dungeons.discovery.parser :as parser]
    [somerville.dungeons.discovery.image :as image]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c])
  (:use clojure.test))

;;==================================================================================================================
;; Debugging helpers

(defn draw-triangle
  "Transform a triangle into a Java graphics polygon and render it."
  [triangle ^Graphics2D graphics]
  (let [xs (into-array Integer/TYPE (list (:x (:p1 triangle)) (:x (:p2 triangle)) (:x (:p3 triangle))))
        ys (into-array Integer/TYPE (list (:y (:p1 triangle)) (:y (:p2 triangle)) (:y (:p3 triangle))))
        p (Polygon. xs ys (count xs))
        tmp (.fillPolygon graphics p)
        tmp (.drawPolygon graphics p)]))

(defn debug-draw
  "Create an image of size width x height with transparency and paint it completely black."
  [^String filename ^Integer width ^Integer height circle points lines active-walls last-point triangles]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics Color/white)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.setPaint graphics Color/black)
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) lines))
        tmp (.setPaint graphics Color/red)
        tmp (.drawOval graphics (- (:x (:p circle)) (:r circle)) (- (:y (:p circle)) (:r circle)) (* 2 (:r circle)) (* 2 (:r circle)))
        tmp (.drawLine graphics (- (:x (:p circle)) 5) (- (:y (:p circle)) 5) (+ (:x (:p circle)) 5) (+ (:y (:p circle)) 5))
        tmp (.drawLine graphics (- (:x (:p circle)) 5) (+ (:y (:p circle)) 5) (+ (:x (:p circle)) 5) (- (:y (:p circle)) 5))
        tmp (.setPaint graphics Color/green)
        tmp (dorun (map #(do
                           (.drawLine graphics (- (:x %) 5) (- (:y %) 5) (+ (:x %) 5) (+ (:y %) 5))
                           (.drawLine graphics (- (:x %) 5) (+ (:y %) 5) (+ (:x %) 5) (- (:y %) 5))) points))
        tmp (.setPaint graphics Color/blue)
        tmp (when-not (nil? last-point)
              (.drawLine graphics (:x last-point) (- (:y last-point) 5) (:x last-point) (+ (:y last-point) 5))
              (.drawLine graphics (- (:x last-point) 5) (:y last-point) (+ (:x last-point) 5) (:y last-point)))
        tmp (.setPaint graphics Color/orange)
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) active-walls))
        tmp (.setPaint graphics Color/gray)
        tmp (dorun (map #(draw-triangle % graphics) triangles))
        tmp (.dispose graphics)]
    (images/write-image filename img)))

(defn debug-text
  [remaining point new-walls current-walls last-walls new-point new-triangles relevant-event]
  (let [starting-walls (filter #(= (:point relevant-event) (:p1 %)) (concat current-walls last-walls new-walls))
        ending-walls (filter #(= (:point relevant-event) (:p2 %)) (concat current-walls last-walls new-walls))
        starting-count (count starting-walls)
        ending-count (count ending-walls)]
    (str
      "========================================\nEVENTS\n"
      (clojure.string/join "\n" (map #(commons/out %) (first remaining)))
      "\n----------------------------------------\nRELEVANT: " (commons/out relevant-event)
      "\n----------------------------------------\nNEW WALLS\n"
      (clojure.string/join "\n" (map #(commons/out %) new-walls))
      "\n----------------------------------------\nLAST WALLS\n"
      (clojure.string/join "\n" (map #(commons/out %) last-walls))
      "\n----------------------------------------\nCURRENT WALLS\n"
      (clojure.string/join "\n" (map #(commons/out %) current-walls))
      "\n----------------------------------------\nSTARTING, ENDING: " starting-count " - " ending-count
      "\n----------------------------------------\nTRIANGLES\n"
      (clojure.string/join "\n" (map #(commons/out %) new-triangles))
      "\n----------------------------------------\nNEW POINT\n" (commons/out new-point))))

;;==================================================================================================================
;; Manual testing with debug output in /tmp/

(defn run-manual-point-test
  [testname i walls point width height visualrange]
  (let [debugmapref (atom {})
        triangles (rcwt/discover-point point walls visualrange debugmapref)]
    (dorun
      (for [[s si] (map #(vector %1 %2) (:steps @debugmapref) (iterate inc 0))]
        (let [filename (str "/tmp/discovery-" testname "-point-" i "-step-" si)
              text (debug-text (:remaining s) (:point s) (:new-walls s) (:walls s) (:last-walls s) (:new-point s) (:new-triangles s) (:relevant s))
              tmp (spit (str filename ".log") text)
              circle (c/circle point visualrange)
              tmp (debug-draw (str filename ".png") width height circle (map :point (first (:remaining s))) walls (:new-walls s) (:new-point s) (:new-triangles s))
              ]
          )))))

(defn run-manual-test
  [testname walls points width height visualrange]
  (try
    (let [wall-lines (parser/parse walls)
          ps (map #(vector (p/point (nth %1 0) (nth %1 1)) %2) points (iterate inc 0))]
      (dorun (map #(run-manual-point-test testname (second %) wall-lines (first %) width height visualrange) ps)))
    (catch Exception e (.printStackTrace e))))

(defn manual-test-rooms
  []
  (let [points '([10 110] [100 120] [190 115] [65 95])
        walls (slurp "test-resources/rooms.walls")]
    (time (run-manual-test "rooms" walls points 500 400 100))))

(defn manual-test-baramzigli
  []
  (let [points '([161 1472] [151 1235] [226 996] [515 981] [663 959] [616 883] [806 740] [1065 885] [848 1035] [768 1302] [957 1339])
        walls (slurp "test-resources/baramzigli.walls")]
    (time (run-manual-test "baramzigli" walls points 1300 1517 300))))

(defn manual-test-clear
  []
  (let [points '([500 500])
        walls ""]
    (time (run-manual-test "clear" walls points 1000 1000 300))))

(defn manual-test-simple-line
  []
  (let [points '([500 500])
        walls "line 250,750 750,750"]
    (time (run-manual-test "simple-line" walls points 1000 1000 300))))

(defn manual-test-vertical-parallel-line
  []
  (let [points '([500 500])
        walls "line 500,0 500,400"]
    (time (run-manual-test "vertical-parallel-line" walls points 1000 1000 300))))

(defn manual-test-casting
  []
  (let [points '([500 500])
        walls "line 0,750 400,750
               line 600,750 1000,750"]
    (time (run-manual-test "casting" walls points 1000 1000 300))))

(defn manual-test-maglubiyet
  []
  (let [points '([137 109])
        walls (slurp "test-resources/maglubiyet.walls")]
    (time (run-manual-test "maglubiyet" walls points 6200 5535 300))))

;(manual-test-clear)
;(manual-test-simple-line)
;(manual-test-vertical-parallel-line)
;(manual-test-casting)
;(manual-test-rooms)
;(manual-test-baramzigli)
;(manual-test-maglubiyet)

