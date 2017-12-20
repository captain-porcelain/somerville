;; Functions to create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon BasicStroke RenderingHints]
    [java.awt.image BufferedImage])
  (:require
    [somerville.commons :as commons]
    [somerville.image :as image]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.polygon :as poly]
    [taoensso.timbre :as log]))

;;==================================================================================================================
;; Parsing wall description

(defn translate-line
  "Translate a line description into an actual line."
  [parameters]
  ;; Check that we have two parameters
  (when (= 2 (count parameters))
    (let [parts (reduce concat (map #(clojure.string/split % #",") parameters))
          iparts (filter #(not (nil? %)) (map #(commons/parse-int % nil) parts))]
      (when (= 4 (count iparts))
        (l/line (p/point (nth iparts 0) (nth iparts 1)) (p/point (nth iparts 2) (nth iparts 3)))))))

(defn translate-description
  "Translate one wall description."
  [^String description]
  (let [parts (clojure.string/split (.trim description) #"\s+")]
    (when (< 1 (count parts))
      (case (first parts)
        "line" (translate-line (rest parts))
        :else nil))))

(defn parse
  "Given a string representation of wall descriptions create the geometrical representations for them."
  [^String wall-description]
  (filter #(not (nil? %)) (map translate-description (clojure.string/split wall-description #"\n"))))

;;==================================================================================================================
;; Drawing helpers

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

(defn draw-triangle
  "Transform a triangle into a Java graphics polygon and render it."
  [triangle graphics]
  (let [xs (into-array Integer/TYPE (list (:x (:p1 triangle)) (:x (:p2 triangle)) (:x (:p3 triangle))))
        ys (into-array Integer/TYPE (list (:y (:p1 triangle)) (:y (:p2 triangle)) (:y (:p3 triangle))))
        p (Polygon. xs ys (count xs))
        tmp (.fillPolygon graphics p)
        tmp (.drawPolygon graphics p)]
    ))

;;==================================================================================================================
;; Debugging helpers

(def debug (atom false))
(def debug-fn-1 (atom nil))
(def debug-fn (atom nil))

(defn debug-draw
  "Create an image of size width x height with transparency and paint it completely black."
  [^String filename ^Integer width ^Integer height circle points lines active-walls last-point triangles]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
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
    (image/write-image filename img)))

(defn make-debug-fn-1
  [discovered-image visualrange]
  (fn
    [filename point points sorted-walls active-walls last-point triangles]
    (debug-draw filename (.getWidth discovered-image) (.getHeight discovered-image) (c/circle point visualrange) points sorted-walls active-walls last-point triangles)))

(defn make-debug-fn-2
  [point sorted-walls]
  (fn
    [filename points active-walls last-point triangles]
    (@debug-fn-1 filename point points sorted-walls active-walls last-point triangles)))

;;==================================================================================================================
;; Geometric discovery
;; Discovery based on ray casting to wall points
;; See https://www.redblobgames.com/articles/visibility/
;;

(defrecord Event [angle point wall angle-2]
  gcommons/Printable
  (gcommons/out [this i] (str (gcommons/indent i) "Event at angle " angle " for\n" (gcommons/out point (inc i)) "\n" (gcommons/out wall (inc i))))
  (gcommons/out [this] (gcommons/out this 0)))

(defn event
  [angle point wall angle-2]
  (Event. angle point wall angle-2))

(defn sort-line-points
  "Reorder the two points of a line so they are sorted by the angle defined by the line point, point and ref-point."
  [line point ref-point]
  (let [points (list (:p1 line) (:p2 line))
        angles (sort (map #(p/angle-pos point % ref-point) points))
        sorted (sort-by #(p/angle-pos point % ref-point) points)]
    (cond
      (and (gcommons/close-to 0 (first angles)) (< Math/PI (second angles))) (l/line (second sorted) (first sorted))
      (and (< (first angles) (/ Math/PI 2)) (> (second angles) (/ (* Math/PI 3) 2))) (l/line (second sorted) (first sorted))
      :else (l/line (first sorted) (second sorted)))))

(defn shorten-walls
  "Reduce the walls to those that are inside the polygon."
  [polygon walls]
  (filter #(not (nil? %)) (map #(poly/shorten-line polygon %) walls)))

(defn relevant-walls
  "Get all walls relevant to the discovered point including limiting
  the distance through polygon approximation of the view circle."
  [point walls visualrange polygon-steps]
  (let [circle-points (c/circle-points (c/circle point visualrange) polygon-steps)
        polygon (poly/from-points circle-points point)
        relevant-walls (shorten-walls polygon (filter #(not (= (:x point) (:x (:p1 %)) (:x (:p2 %)))) walls))]
    (concat (:lines polygon) relevant-walls)))

(defn gather-events
  "Create paritioned list of angles of the wall points.
  Each partition contains the points on that angle sorted by the distance to the discovery point."
  [walls point ref-point]
  (map
    (fn [angle-points]
      (sort-by #(p/distance point (:point %)) angle-points))
    (partition-by :angle
      (sort-by :angle
        (reduce concat
          (map
            #(let [a1 (p/angle-pos point (:p1 %) ref-point)
                   a2 (p/angle-pos point (:p2 %) ref-point)]
               (list
                 (event a1 (:p1 %) % a2)
                 (event a2 (:p2 %) % a1)))
            walls))))))

(defn relevant-event
  "Given the center point of the discovery and the events at a given angle return the relevant event."
  [point events]
  (first (sort-by #(p/distance point (:point %)) events)))

(defn relevant-wall
  "Get the closest wall to point of discovery."
  [point relevant-point walls]
  (let [line (l/line point relevant-point)]
    (first (sort-by #(p/distance point (if (l/parallel? line %)
                                         (first (sort-by (fn [lp] (p/distance point lp)) (list (:p1 %) (:p2 %))))
                                         (l/intersect line %))) walls))))

(defn consider-event?
  "Check if an event is visible."
  [point event walls]
  (let [line (l/line point (:point event))
        dist (p/distance point (:point event))
        intersections (filter #(not (nil? %)) (map #(l/intersect-segments line %) walls))
        closer (filter #(and (not (gcommons/close-to dist (p/distance point %))) (< (p/distance point %) dist)) intersections)]
    (= 0 (count closer))))

(defn cast-point
  "Find the next triangle point either at the event or at the wall behind the event."
  [point event walls]
  (let [other-walls (filter #(not (or (= (:point event) (:p1 %)) (= (:point event) (:p2 %)))) walls)
        ws (if (= 0 (count other-walls)) walls other-walls)]
    (first (sort-by #(p/distance point %) (l/cuts (l/line point (:point event)) ws)))))

(defn active-walls
  "Get the list of active walls based on the events at an angle."
  [current-walls events]
  (let [ps (map :point events)]
    (concat
      (filter #(not (commons/in? (:p2 %) ps)) current-walls)
      (distinct (filter #(commons/in? (:p1 %) ps) (map :wall events))))))

(defn all-lines-upcoming?
  "Check if all walls adjacent to the relevant event are still coming up in the processing."
  [event current-events]
  (let [adjacent-walls (filter #(or (= (:point event) (:p1 (:wall %))) (= (:point event) (:p2 (:wall %)))) current-events)
        angles (reduce concat (map #(list (:angle %) (:angle-2 %)) adjacent-walls))]
    (every? #(or (> (:angle event) %) (gcommons/close-to (:angle event) %)) angles)))

(defn update-triangles
  "This is the function where most of the logic sits. It represents the processing of the events at one angle.
  It finds next point of a triangle, and updates relevant walls and adds triangles."
  [point last-point current-triangles current-walls current-events remaining]
  (let [event (relevant-event point current-events)
        wall (relevant-wall point (:point event) current-walls)
        new-walls (active-walls current-walls current-events)
        bigger (all-lines-upcoming? event current-events)]
    (cond
    (or (nil? last-point) (= 0 (count current-walls)))
      [(:point event) current-triangles new-walls]
    (or (consider-event? point event current-walls) (= 0 remaining))
      (let [p (cast-point point event current-walls)
            triangle-point (if bigger (:point event) p)
            new-point (if bigger p (:point event))
            triangle (t/triangle point last-point triangle-point)]
        [new-point (conj current-triangles triangle) new-walls])
    :else
      [last-point current-triangles new-walls])))

(defn debug-visible-triangles
  [remaining point new-walls new-point new-triangles]
  (dorun (println "========================================\nevents"))
  (dorun (map #(println (gcommons/out %)) (first remaining)))
  (dorun (println "----------------------------------------"))
  (dorun (println (str "relevant : " (gcommons/out (relevant-event point (first remaining))))))
  (dorun (println "----------------------------------------\nwalls"))
  (dorun (map #(println (gcommons/out %)) new-walls))
  (dorun (println "----------------------------------------\ntriangles"))
  (dorun (println (str "new point: " (gcommons/out new-point))))
  (dorun (map #(println (gcommons/out %)) new-triangles)))

(defn visible-triangles
  "Find the visible triangles."
  [point events]
  (let [event-line (l/line point (:point (relevant-event point (first events))))
        all-walls (map :wall (reduce concat events))
        i (first (sort-by #(p/distance point %) (l/cuts-segments event-line all-walls)))
        intersected-walls (filter #(l/point-on-segment? % i) all-walls)
        starting-walls (if (nil? i) (list) intersected-walls)
        tmp (when-not (nil? @debug-fn) (@debug-fn (str "/tmp/discovery-step-" (System/currentTimeMillis) ".png") (map :point (first events)) starting-walls i (list)))
        ]
    (loop [remaining events
           last-point i
           walls starting-walls
           triangles (list)]
      (if (= 0 (count remaining))
        triangles
        (let [[new-point new-triangles new-walls] (update-triangles point last-point triangles walls (first remaining) (count (rest remaining)))
              tmp (when @debug (debug-visible-triangles remaining point new-walls new-point new-triangles))
              tmp (when @debug (@debug-fn (str "/tmp/discovery-step-" (System/currentTimeMillis) ".png") (map :point (first remaining)) new-walls new-point new-triangles))
              ]
          (recur
            (rest remaining)
            new-point
            new-walls
            new-triangles))))))

(defn discover-point
  "Discover the visible area created by one discovered point."
  [point walls visualrange graphics]
  (let [polygon-steps 16
        ref-point (p/point -1 (:y point))
        sorted-walls (map #(sort-line-points % point ref-point) (relevant-walls point walls visualrange polygon-steps))
        events (gather-events sorted-walls point ref-point)
        events (concat events (list (first events)))
        tmo (when-not (nil? @debug-fn-1) (reset! debug-fn (make-debug-fn-2 point sorted-walls)))
        triangles (visible-triangles point events)
        tmp (dorun (map #(draw-triangle % graphics) triangles))]))

(defn discover-ray-casting
  "Discover the visible areas based on ray casting with wall tracing."
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [polygon-steps 16
        ps (map #(p/point (nth % 0) (nth % 1)) points)
        graphics ^Graphics2D (.createGraphics discovered-image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (.setStroke graphics (BasicStroke. 2))
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (when @debug (reset! debug-fn-1 (make-debug-fn-1 discovered-image visualrange)))
        tmp (dorun (map #(discover-point % wall-lines visualrange graphics) ps))
        tmp (.dispose graphics)]
    discovered-image))


;;==================================================================================================================
;; Discovery Interfaces
;;

(defn discover
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  [points wall-description width height visualrange]
  (let [discovered-image (create-undiscovered-graphics width height)
        wall-lines (parse wall-description)
        tmp (do (discover-ray-casting points wall-lines discovered-image visualrange))]
    discovered-image))
