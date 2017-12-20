;; Functions to create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon]
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
        p (Polygon. xs ys (count xs))]
    (.fillPolygon graphics p)))

;;==================================================================================================================
;; Debugging helpers

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
        tmp (.setPaint graphics Color/yellow)
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) active-walls))
        tmp (.setPaint graphics Color/gray)
        tmp (dorun (map #(draw-triangle % graphics) triangles))
        tmp (.dispose graphics)]
    (image/write-image filename img)))

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
  ;;TODO check for lines that cross the line from point to point-ref
  [line point ref-point]
  (let [points (list (:p1 line) (:p2 line))
        angles (sort (map #(p/angle-pos point % ref-point) points))
        sorted (sort-by #(p/angle-pos point % ref-point) points)]
    (if (and (gcommons/close-to 0 (first angles)) (< Math/PI (second angles)))
      (l/line (second sorted) (first sorted))
      (l/line (first sorted) (second sorted)))))

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
        relevant-walls (shorten-walls polygon walls)]
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
        closer (filter #(and (not (gcommons/close-to dist (p/distance point %))) (< (p/distance point %) dist)) intersections)
        ;tmp (dorun (println (str "Event point: " (gcommons/out (:point event)))))
        ;tmp (dorun (println (str "Walls: " (count walls))))
        ;tmp (dorun (map #(println (gcommons/out %)) intersections))
        ;first-intersection (and (< 0 (count intersections)) (gcommons/close-to 0 (p/distance (:point event) (first intersections))))
        tmp (dorun (println (str "Considering event: " (count closer))))]
    (= 0 (count closer))))

(defn cast-point
  "Find the next triangle point either at the event or at the wall behind the event."
  [point event walls]
  (let [other-walls (filter #(not (or (= (:point event) (:p1 %)) (= (:point event) (:p2 %)))) walls)
        ws (if (= 0 (count other-walls)) walls other-walls)]
    (first
    (sort-by
      #(p/distance point %)
      (l/cuts (l/line point (:point event)) ws)))))

(defn active-walls
  "Get the list of active walls based on the events at an angle."
  [current-walls events]
  (let [ps (map :point events)]
    (concat
      (filter #(not (commons/in? (:p2 %) ps)) current-walls)
      (distinct (filter #(commons/in? (:p1 %) ps) (map :wall events))))))

(defn edge?
  "Check if the current event is an edge."
  [event current-events]
  (let [adjacent-walls (filter #(or (= (:point event) (:p1 (:wall %))) (= (:point event) (:p2 (:wall %)))) current-events)
        angles (reduce concat (map #(list (:angle %) (:angle-2 %)) adjacent-walls))
        smaller (every? #(or (< (:angle event) %) (gcommons/close-to (:angle event) %)) angles)
        bigger  (every? #(or (> (:angle event) %) (gcommons/close-to (:angle event) %)) angles)]
    [smaller bigger (or smaller bigger)]))

(defn update-triangles
  "This is the function where most of the logic sits. It represents the processing of the events at one angle.
  It finds next point of a triangle, and updates relevant walls and adds triangles."
  [point last-point current-triangles current-walls current-events]
  (let [event (relevant-event point current-events)
        wall (relevant-wall point (:point event) current-walls)
        new-walls (active-walls current-walls current-events)
        [smaller bigger edge] (edge? event current-events)]
    (cond
    (or (nil? last-point) (= 0 (count current-walls)))
      [(:point event) current-triangles new-walls]
    (consider-event? point event current-walls)
      (let [p (cast-point point event current-walls)
            triangle-point (if bigger (:point event) p)
            new-point (if bigger p (:point event))
            triangle (t/triangle point last-point triangle-point)]
        [new-point (conj current-triangles triangle) new-walls])
    :else
      [last-point current-triangles new-walls])))

(defn visible-triangles
  "Find the visible triangles."
  [point events debug-fn]
  (let [event-line (l/line point (:point (relevant-event point (first events))))
        all-walls (map :wall (reduce concat events))
        i (first (sort-by #(p/distance point %) (l/cuts-segments event-line all-walls)))
        intersected-walls (filter #(l/point-on-segment? % i) all-walls)
        starting-walls (if (nil? i) (list) intersected-walls)
        tmp (dorun (println (str (gcommons/out i) " - " (count intersected-walls))))
        tmp (when-not (nil? debug-fn) (debug-fn (str "/tmp/discovery-step-" (System/currentTimeMillis) ".png") (map :point (first events)) starting-walls i (list)))]
    (loop [remaining events
           last-point i
           walls starting-walls
           triangles (list)]
      (if (= 0 (count remaining))
        triangles
        (let [[new-point new-triangles new-walls] (update-triangles point last-point triangles walls (first remaining))
              tmp (when-not (nil? debug-fn) (debug-fn (str "/tmp/discovery-step-" (System/currentTimeMillis) ".png") (map :point (first remaining)) new-walls new-point new-triangles))]
          (recur
            (rest remaining)
            new-point
            new-walls
            new-triangles))))))

(defn discover-rays
  "Discover the visible area based on ray casting with wall tracing."
  [point walls visualrange graphics debug-fn]
  (let [polygon-steps 16
        ref-point (p/point -1 (:y point))
        sorted-walls (map #(sort-line-points % point ref-point) (relevant-walls point walls visualrange polygon-steps))
        events (gather-events sorted-walls point ref-point)
        events (concat events (list (first events)))
        debug-fn2 (if (not (nil? debug-fn)) (fn [filename points active-walls last-point triangles] (debug-fn filename point points sorted-walls active-walls last-point triangles)) nil)
        triangles (visible-triangles point events debug-fn2)
        tmp (dorun (map #(draw-triangle % graphics) triangles))]))

(defn discover-ray-casting
  "Discover the visible areas based on ray casting with wall tracing."
  [points wall-lines ^BufferedImage discovered-image visualrange debug]
  (let [polygon-steps 16
        ps (map #(p/point (nth % 0) (nth % 1)) points)
        graphics ^Graphics2D (.createGraphics discovered-image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        debug-fn (if debug (fn [filename point points sorted-walls active-walls last-point triangles] (debug-draw filename (.getWidth discovered-image) (.getHeight discovered-image) (c/circle point visualrange) points sorted-walls active-walls last-point triangles)) nil)
        tmp (dorun (map #(discover-rays % wall-lines visualrange graphics debug-fn) ps))
        tmp (.dispose graphics)]
    discovered-image))


;;==================================================================================================================
;; Discovery Interfaces
;;

(defn discover
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  ([points wall-description width height visualrange debug]
   (let [discovered-image (create-undiscovered-graphics width height)
         wall-lines (parse wall-description)
         tmp (do (discover-ray-casting points wall-lines discovered-image visualrange debug))]
     discovered-image))
  ([points wall-description width height visualrange]
   (discover points wall-description width height visualrange false)))
