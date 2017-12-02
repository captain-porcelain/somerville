(ns somerville.geometry.polygon
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]))

;; Define a polygon as a list of lines and an optional center.
;; This polygon is specifically used for calculating visibility.
(defrecord Polygon2 [lines center]
  c/Printable
  (c/out [this i] (str (c/indent i) "Polygon of lines:\n" (clojure.string/join "\n" (map #(c/out % (inc i)) lines))))
  (c/out [this] (c/out this 0)))

(defn polygon
  "Get a line from two points."
  ([lines center]
   (Polygon2. lines center))
  ([lines]
   (Polygon2. lines nil)))

(defn from-points
  "Create a polygon from a list of points."
  ([points center]
   (polygon (map #(l/line %1 %2) points (concat (rest points) (list (first points)))) center))
  ([points]
   (from-points points nil)))

(defn intersect
  "Intersect a polygon with a line."
  [polygon line]
  (filter #(not (nil? %)) (map #(l/intersect % line) (:lines polygon))))

(defn cast-to
  "Create a line from the given point to the polygon by casting a ray from the center through the given point."
  [polygon point]
  (let [ray (l/line (:center polygon) point)
        i (first (intersect polygon ray))]
    (l/line i point)))

(defn cut-new-lines
  [polygon line intersected]
  (dorun (println (str "case: " (count intersected))))
  (case (count intersected)
    2 (list
        (l/line (:p1 (first (first intersected))) (second (first intersected)))
        (l/line (second (first intersected)) (second (second intersected)))
        (l/line (second (second intersected)) (:p2 (first (second intersected)))))
    1 (let [in-point (first (sort-by #(p/distance (:center polygon %)) (map second intersected)))
            ray (cast-to polygon in-point)]
        )
    :else (list)))

(defn visible?
  [polygon-line wall center]
  (and (nil? (l/intersect-segments wall (l/line center (:p1 polygon-line))))
       (nil? (l/intersect-segments wall (l/line center (:p2 polygon-line))))))

(defn cut
  "Cut polygon by intersecting with a line segment. Four cases are possible:
  - Line segment doesn't intersect polygon at all -> do nothing
  - Line segment is contained in circle -> get intersections by casting lines from center over segment start and end
                                           to polygon lines, thus introducing three new lines.
  - Line has one intersection -> cast a line to the contained segment endpoint to the polygon lines. Introduces two new lines.
  - Line has two intersections -> introduce one new line."
  [polygon line]
  (let [intersections (map #(vector % (l/intersect % line)) (:lines polygon))
        intersected (filter #(not (nil? (second %))) intersections)
        new-lines (cut-new-lines polygon line intersected)
        start (map first (take-while #(visible? (first %) line (:center polygon)) intersections))
        end (map first (reverse (take-while #(visible? (first %) line (:center polygon)) (reverse intersections))))]
    (Polygon2. (concat start new-lines end) (:center polygon))))
