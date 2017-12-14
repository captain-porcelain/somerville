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

(defn relevant-lines
  "Filter given lines to those that intersect the circle or are contained by it."
  [circle lines]
  (filter #(or
             (c/point-in? circle (:p1 %))
             (c/point-in? circle (:p2 %))
             (< 0 (count (c/intersect-line-segment circle %))))
          lines))

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
;; Discovery based on circle rasterization and line cuts from center to circle points
;;

(defn discover-circle
  "Given a point of origin, check all pixels on the circle given by this origin for
  intersections of the line from origin to the pixel with any wall lines.
  Find the outermost visible pixel and consider the line from the center to the pixel as visible."
  [wall-lines ^Graphics2D graphics visualrange origin circle-base-points]
  (let [center (p/point (nth origin 0) (nth origin 1))
        circle (c/circle center visualrange)
        relevant-walls (relevant-lines circle wall-lines)
        tmp (dorun (println (str "Circle intersects lines: " (count relevant-walls))))
        circle-points (rasterize/translate-line circle-base-points origin)]
    (dorun
      (for [[x y] circle-points]
        (let [point (p/point x y)
              center-line (l/line center point)
              intersections (filter #(not (nil? %)) (map #(l/intersect-segments center-line %) relevant-walls))
              nearest (first (sort-by #(p/distance % center) intersections))
              visible-to (if (nil? nearest) point nearest)
              tmp (.drawLine graphics (:x visible-to) (:y visible-to) (nth origin 0) (nth origin 1))]
          graphics)))))

(defn discover-circles
  "Given a point of origin, check all pixels on the circle given by this origin for
  intersections of the line from origin to the pixel with any wall lines.
  Find the outermost visible pixel and consider the line from the center to the pixel as visible."
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [circle-base-points (rasterize/circle visualrange)
        tmp (dorun (println (str "Discovered points: " (count points))))
        tmp (dorun (println (str "Circle base points: " (count circle-base-points))))
        graphics ^Graphics2D (.createGraphics discovered-image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (dorun (map #(discover-circle wall-lines graphics visualrange % circle-base-points) points))
        tmp (.dispose graphics)]
    discovered-image))


;;==================================================================================================================
;; Discovery based on representing circle as polygon and cutting it with walls.
;;

(defn cut-polygon
  [polygon wall-lines]
  (loop [cut polygon
         lines wall-lines]
    (if (= 0 (count lines))
      cut
      (recur (poly/cut cut (first lines)) (rest lines)))))

(defn discover-polygon
  [polygon wall-lines graphics]
  (let [cut (cut-polygon polygon wall-lines)
        xs (into-array Integer/TYPE (map #(:x (:p1 %)) (:lines cut)))
        ys (into-array Integer/TYPE (map #(:y (:p1 %)) (:lines cut)))
        p (Polygon. xs ys (count xs))]
    (.drawPolygon graphics p)))

(defn discover-polygons
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [polygon-steps 16
        ps (map #(p/point (nth % 0) (nth % 1)) points)
        polygons (map #(poly/from-points (c/circle-points (c/circle % visualrange) polygon-steps) %) ps)
        tmp (dorun (println (str "Discovered points: " (count points))))
        tmp (dorun (println (str "Circle lines: " polygon-steps)))
        graphics ^Graphics2D (.createGraphics discovered-image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (dorun (map #(discover-polygon % wall-lines graphics) polygons))
        tmp (.dispose graphics)]
    discovered-image))


;;==================================================================================================================
;; Discovery based on ray casting to wall points
;; See https://www.redblobgames.com/articles/visibility/

(defrecord Triangle [p1 p2 p3])

(defrecord Event [angle point wall]
  gcommons/Printable
  (gcommons/out [this i] (str (gcommons/indent i) "Event at angle " angle " for\n" (gcommons/out point (inc i)) "\n" (gcommons/out wall (inc i))))
  (gcommons/out [this] (gcommons/out this 0)))

(defn event
  [angle point wall]
  (Event. angle point wall))

(defn sort-line-points
  "Change line points so they are sorted by the angle defined by line point point and ref-point."
  [line point ref-point]
  (let [points (list (:p1 line) (:p2 line))
        sorted (sort-by #(p/angle-pos point % ref-point) points)]
    (l/line (first sorted) (second sorted))))

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

(defn angle-points-walls
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

(defn update-triangles
  [current-triangles point-walls]
  )

(defn active-walls
  [current-walls point-walls]
  (let [ps (map :point point-walls)]
    (concat
      (filter #(not (commons/in? (:p2 %) ps)) current-walls)
      (distinct (filter #(commons/in? (:p1 %) ps) (map :wall point-walls))))))

(defn visible-triangles
  "Find the visible triangles."
  [point angles]
  (loop [remaining angles
         walls (list)
         triangles (list)]
    (if (= 0 (count remaining))
      triangles
      (recur
        (rest angles)
        (active-walls walls (first angles))
        (update-triangles triangles (first angles))))))

(defn discover-rays
  "Discover the visible area based on ray casting with wall tracing."
  [point walls visualrange graphics]
  (let [polygon-steps 16
        ref-point (p/point -1 0)
        sorted-walls (map #(sort-line-points % point ref-point) (relevant-walls point walls visualrange polygon-steps))
        angles (angle-points-walls sorted-walls point ref-point)
        ]
    ))

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
        tmp (do (discover-circles points wall-lines discovered-image visualrange))]
        ;tmp (do (discover-polygons points wall-lines discovered-image visualrange))]
    discovered-image))

(defn discover-poly
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  [points wall-description width height visualrange]
  (let [discovered-image (create-undiscovered-graphics width height)
        wall-lines (parse wall-description)
        tmp (do (discover-polygons points wall-lines discovered-image visualrange))]
    discovered-image))

