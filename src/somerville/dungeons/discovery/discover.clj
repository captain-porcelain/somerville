;; Functions to create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discover
  (:require
    [somerville.commons :as commons]
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

(defn intersections
  [circle lines]
  (map #(hash-map :line % :intersections (c/intersect-line circle %)) lines))

