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

(defn intersect-segments
  "Intersect a polygon with a line."
  [polygon line]
  (filter #(not (nil? %)) (map #(l/intersect-segments % line) (:lines polygon))))

(defn cast-shadow
  "Find the point on the polygon that is shadowed by a wall by casting a ray from the center through the endpoint of the wall that is inside the polygon."
  [polygon point]
  (let [ray (l/line (:center polygon) point)
        intersections (intersect polygon ray)]
    (first (sort-by #(p/distance point %) intersections))))

(defn shadow-on-wall
  [polygon wall-point]
  (= 0 (count (intersect-segments polygon (l/line (:center polygon) wall-point)))))

(defn update-intersections
  "Given the intersections (as polygon line and intersection point pairs) update these in case the line is partially or completely contained in the polygon."
  [polygon line intersections]
  (let [intersected (filter #(not (nil? (second %))) intersections)
        tmp (dorun (println (str "case: " (count intersected))))]
    (case (count intersected)
      1 (let [in-point (first (filter #(shadow-on-wall polygon %) (list (:p1 line) (:p2 line))))
              tmp (dorun (println (str "in-point " (c/out in-point))))
              shadow-point (cast-shadow polygon in-point)
              tmp (dorun (println (str "shadow-point " (c/out shadow-point))))
              ]
          (map #(if (l/point-on-segment? (first %) shadow-point) [(first %) shadow-point] %) intersections))
      0 (let [shadow-point-1 (cast-shadow polygon (:p1 line))
              shadow-point-2 (cast-shadow polygon (:p2 line))
              tmp (dorun (println (str "shadow-point 1 " (c/out shadow-point-1))))
              tmp (dorun (println (str "shadow-point 2 " (c/out shadow-point-2))))
              ]
          (map #(cond
                  (l/point-on-segment? (first %) shadow-point-1) [(first %) shadow-point-1]
                  (l/point-on-segment? (first %) shadow-point-2) [(first %) shadow-point-2]
                  :else %)
               intersections))
      intersections)))

(defn cut-new-lines
  [polygon line intersections]
  (let [update-intersections (update-intersections polygon line intersections)
        intersected (filter #(not (nil? (second %))) intersections)]
      (list
        (l/line (:p1 (first (first intersected))) (second (first intersected)))
        (l/line (second (first intersected)) (second (second intersected)))
        (l/line (second (second intersected)) (:p2 (first (second intersected)))))))

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
  (let [intersections (map #(vector % (l/intersect-segments % line)) (:lines polygon))
        new-lines (cut-new-lines polygon line intersections)
        start (map first (take-while #(visible? (first %) line (:center polygon)) intersections))
        end (map first (reverse (take-while #(visible? (first %) line (:center polygon)) (reverse intersections))))]
    (Polygon2. (concat start new-lines end) (:center polygon))))
