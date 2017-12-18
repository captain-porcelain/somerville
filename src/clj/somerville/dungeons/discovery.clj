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
    [somerville.geometry.polygon :as poly]
    [somerville.geometry.rasterize :as rasterize]
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
;; Geometric discovery
;;

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

;;==================================================================================================================
;; Discovery based on ray casting to wall points
;; See https://www.redblobgames.com/articles/visibility/

(defrecord Triangle [p1 p2 p3]
  gcommons/Printable
  (gcommons/out [this i] (str (gcommons/indent i) "Triangle of points\n" (gcommons/out p1 (inc i)) "\n" (gcommons/out p2 (inc i)) "\n" (gcommons/out p3 (inc i))))
  (gcommons/out [this] (gcommons/out this 0)))

(defn triangle
  [p1 p2 p3]
  (Triangle. p1 p2 p3))

(defrecord Event [angle point wall]
  gcommons/Printable
  (gcommons/out [this i] (str (gcommons/indent i) "Event at angle " angle " for\n" (gcommons/out point (inc i)) "\n" (gcommons/out wall (inc i))))
  (gcommons/out [this] (gcommons/out this 0)))

(defn event
  [angle point wall]
  (Event. angle point wall))

(defn sort-line-points
  "Reorder the two points of a line so they are sorted by the angle defined by the line point, point and ref-point."
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
            #(list
               (event (p/angle-pos point (:p1 %) ref-point) (:p1 %) %)
               (event (p/angle-pos point (:p2 %) ref-point) (:p2 %) %))
            walls))))))

(defn relevant-event
  "Given the center point of the discovery and the events at a given angle return the relevant event."
  [point events]
  (first (sort-by #(p/distance point (:point %)) events)))

(defn relevant-wall
  "Get the closest wall to point of discovery."
  [point relevant-point walls]
  (let [line (l/line point relevant-point)]
    (first (sort-by #(p/distance point (l/intersect line %)) walls))))

(defn consider-event?
  "Check if an event is visible."
  [point event walls]
  (let [line (l/line point (:point event))]
    (= 0 (count (filter #(not (nil? %)) (map #(l/intersect-segments line %) walls))))))

(defn next-point
  "Find the next triangle point either at the event or at the wall behind the event."
  [point event-point new-walls]
  (if (= 0 (count (filter #(= event-point (:p1 %)) new-walls)))
    (first (sort-by #(p/distance point %) (l/cuts (l/line point event-point) new-walls)))
    event-point))

(defn active-walls
  "Get the list of active walls based on the events at an angle."
  [current-walls events]
  (let [ps (map :point events)]
    (concat
      (filter #(not (commons/in? (:p2 %) ps)) current-walls)
      (distinct (filter #(commons/in? (:p1 %) ps) (map :wall events))))))

(defn update-triangles
  "This is the function where most of the logic sits. It represents the processing of the events at one angle.
  It finds next point of a triangle, and updates relevant walls and adds triangles."
  [point last-point current-triangles current-walls current-events]
  (let [event (relevant-event point current-events)
        wall (relevant-wall point (:point event) current-walls)
        new-walls (active-walls current-walls current-events)
        other-walls (filter #(not (= wall %)) new-walls)]
    (cond
    (or (nil? last-point) (= 0 (count current-walls)))
      [(:point event) current-triangles new-walls]
    (consider-event? point event (filter #(not (= wall %)) current-walls))
      (let [triangle (triangle point last-point (:point event))
            p (next-point point (:point event) new-walls)]
        [p (conj current-triangles triangle) new-walls])
    :else
      [last-point current-triangles new-walls])))

(defn visible-triangles
  "Find the visible triangles."
  [point events]
  (loop [remaining events
         last-point nil
         walls (list)
         triangles (list)]
    (if (= 0 (count remaining))
      triangles
      (let [[new-point new-triangles new-walls] (update-triangles point last-point triangles walls (first remaining))]
        (recur
          (rest remaining)
          new-point
          new-walls
          new-triangles)))))

(defn draw-triangle
  "Transform a triangle into a Java graphics polygon and render it."
  [triangle graphics]
  (let [xs (into-array Integer/TYPE (list (:x (:p1 triangle)) (:x (:p2 triangle)) (:x (:p3 triangle))))
        ys (into-array Integer/TYPE (list (:y (:p1 triangle)) (:y (:p2 triangle)) (:y (:p3 triangle))))
        p (Polygon. xs ys (count xs))]
    (.fillPolygon graphics p)))

(defn discover-rays
  "Discover the visible area based on ray casting with wall tracing."
  [point walls visualrange graphics]
  (let [polygon-steps 16
        ref-point (p/point -1 (:y point))
        sorted-walls (map #(sort-line-points % point ref-point) (relevant-walls point walls visualrange polygon-steps))
        events (gather-events sorted-walls point ref-point)
        events (concat events (list (first events)))
        triangles (visible-triangles point events)
        tmp (dorun (map #(draw-triangle % graphics) triangles))]))

(defn discover-ray-casting
  "Discover the visible areas based on ray casting with wall tracing."
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [polygon-steps 16
        ps (map #(p/point (nth % 0) (nth % 1)) points)
        tmp (dorun (println (str "Discovered points: " (count points))))
        graphics ^Graphics2D (.createGraphics discovered-image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (dorun (map #(discover-rays % wall-lines visualrange graphics) ps))
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

