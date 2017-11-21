;; Functions to create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discover
  (:import
    [java.awt Color Graphics2D Rectangle]
    [java.awt.image BufferedImage])
  (:require
    [somerville.commons :as commons]
    [somerville.image :as image]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
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
;; Have a list of lines and filter them by having intersections with the current view circle.
;; Next check the pixels in the bounding box for those that are
;; - inside the view circle
;; - do not intersect any relevant wall lines when drawing a line from the circle center to the pixel
;;
;; Possible improvements are to check the overlap between circles and remove them if possible to reduce
;; the amount of pixels to check.
;; Could that be done by instead checking the complete image pixels?
;; Maybe do both and decide upon the strategy by calculating the amount of pixels to check for each strategy.

;;------------------------------------------------------------------------------------------------------------------
;; Helpers

(defn relevant-lines
  "Filter given lines to those that intersect the circle."
  [circle lines]
  (filter #(or
             (c/point-in? circle (:p1 %))
             (c/point-in? circle (:p2 %))
             (< 0 (count (c/intersect-line-segment circle %))))
          lines))

(defn map-circle-lines
  "Map which circle intersects which lines."
  [circles lines]
  (into {} (map (fn [circle] [circle (relevant-lines circle lines)]) circles)))

(defn circle-intersected?
  "Check if line from circle center to point intersects any of the wall lines"
  [circle point wall-lines]
  (let [center-line (l/line (:p circle) point)
        intersections (filter #(not (nil? %)) (map #(l/intersect-segments center-line %) wall-lines))]
    (= 0 (count intersections))))

;;------------------------------------------------------------------------------------------------------------------
;; Discovery

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

(defn discover-box
  "Given a point of origin, check all pixels in the bounding box around the circle given by this origin for:
  - pixel is inside the circle
  - line from origin to pixel does not intersect any wall lines
  If this is true the pixel is considered visible and set to transparent."
  [wall-lines ^BufferedImage discovered-image visualrange origin]
  (let [x-1 (max 0 (- (nth origin 0) visualrange))
        y-1 (max 0 (- (nth origin 1) visualrange))
        x-2 (min (+ (nth origin 0) visualrange 1) (dec (.getWidth discovered-image)))
        y-2 (min (+ (nth origin 1) visualrange 1) (dec (.getHeight discovered-image)))
        free-rgb (.getRGB (Color. 0 0 0 1))
        center (p/point (nth origin 0) (nth origin 1))
        circle (c/circle center visualrange)
        relevant-walls (relevant-lines circle wall-lines)
        tmp (dorun
              (for [x (range x-1 x-2)
                    y (range y-1 y-2)]
                (let [point (p/point x y)
                      in-circle (c/point-in? circle point)
                      center-line (l/line center (p/point x y))
                      intersections (filter #(not (nil? %)) (map #(l/intersect-segments center-line %) relevant-walls))]
                  (when (and (= 0 (count intersections)) in-circle)
                    (.setRGB discovered-image x y free-rgb)))))]
    discovered-image))

(defn discover-bounding-boxes
  "Set all visible pixels in the bounding boxes of the given points to transparent."
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [tmp (dorun (map #(discover-box wall-lines discovered-image visualrange %) points))]
    discovered-image))

(defn discover-all
  "Set all visible pixels in the image to transparent."
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [circles (map #(c/circle (p/point (nth % 0) (nth % 1)) visualrange) points)
        circle-lines (map-circle-lines circles wall-lines)
        tmp (dorun (map (fn [[k v]] (dorun (println (str "Circle intersects lines: " (count v))))) circle-lines))
        free-rgb (.getRGB (Color. 0 0 0 1))
        tmp (dorun
              (for [x (range (.getWidth discovered-image))
                    y (range (.getHeight discovered-image))]
                (let [point (p/point x y)
                      relevant-circles (filter #(c/point-in? % point) circles)
                      visible-in (filter #(circle-intersected? % point (get circle-lines % (list))) relevant-circles)]
                  (when (< 0 (count visible-in))
                    (.setRGB discovered-image x y free-rgb)))))]
    discovered-image))

(defn discover
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  [points wall-lines width height visualrange]
  (let [discovered-image (create-undiscovered-graphics width height)

        points-by-bounding-boxes (* (count points) (* 4 visualrange visualrange))
        points-by-width-height (* width height)

        tmp (dorun (println (str "Points to check by bounding boxes: " points-by-bounding-boxes)))
        tmp (dorun (println (str "Points to check by width and height: " points-by-width-height)))

        tmp (if (< points-by-bounding-boxes points-by-width-height)
              (discover-bounding-boxes points wall-lines discovered-image visualrange)
              (discover-all points wall-lines discovered-image visualrange))]
    discovered-image))
