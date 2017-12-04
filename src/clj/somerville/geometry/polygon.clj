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

(defn point-inside-polygon?
  "Check if point is inside the polygon."
  [polygon point]
  (= 0 (count (intersect-segments polygon (l/line (:center polygon) point)))))

(defn intersection-count
  [intersections]
  (reduce + (map #(count (:intersections %)) intersections)))

(defn update-intersections
  "Given the intersections (as polygon line and intersection point pairs) update these in case the line is partially or completely contained in the polygon."
  [polygon line intersections]
  (let [intersection-count (intersection-count intersections)
        ;tmp (dorun (println (str "case: " intersection-count)))
        ]
    (case intersection-count
      1 (let [in-point (first (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line))))
              ;tmp (dorun (println (str "in-point " (c/out in-point))))
              shadow-point (cast-shadow polygon in-point)
              ;tmp (dorun (println (str "shadow-point " (c/out shadow-point))))
              ]
          (map #(if (l/point-on-segment? (:line %) shadow-point) (assoc % :intersections (conj (:intersections %) shadow-point)) %) intersections))
      0 (let [shadow-point-1 (assoc (cast-shadow polygon (:p1 line)) :wall-point (:p1 line))
              shadow-point-2 (assoc (cast-shadow polygon (:p2 line)) :wall-point (:p2 line))
              ;tmp (dorun (println (str "shadow-point 1 " (c/out shadow-point-1))))
              ;tmp (dorun (println (str "shadow-point 2 " (c/out shadow-point-2))))
              ]
          (map #(let [result %
                      result (if (l/point-on-segment? (:line result) shadow-point-1) (assoc result :intersections (conj (:intersections result) shadow-point-1)) result)
                      result (if (l/point-on-segment? (:line result) shadow-point-2) (assoc result :intersections (conj (:intersections result) shadow-point-2)) result)]
                  result)
               intersections))
      intersections)))

(defn cut-new-lines
  [polygon line intersections updated-intersections]
  (let [intersected (filter #(< 0 (count (:intersections %))) intersections)
        updated-intersected (filter #(< 0 (count (:intersections %))) updated-intersections)]
    (cond
      (= 2 (count intersected))
      (list
        (l/line (:p1 (:line (first intersected))) (first (:intersections (first intersected))))
        (l/line (first (:intersections (first intersected))) (first (:intersections (second intersected))))
        (l/line (first (:intersections (second intersected))) (:p2 (:line (second intersected)))))
      (and (= 0 (count intersected)) (= 1 (count updated-intersected)))
      (list
        (l/line (:p1 (:line (first updated-intersected))) (first (:intersections (first updated-intersected))))
        (l/line (first (:intersections (first updated-intersected))) (:wall-point (first (:intersections (first updated-intersected)))))
        (l/line (:wall-point (first (:intersections (first updated-intersected)))) (:wall-point (second (:intersections (first updated-intersected)))))
        (l/line (:wall-point (second (:intersections (first updated-intersected)))) (second (:intersections (first updated-intersected))))
        (l/line (second (:intersections (first updated-intersected))) (:p2 (:line (first updated-intersected)))))
      (and (= 0 (count intersected)) (= 2 (count updated-intersected)))
      (list
        (l/line (:p1 (:line (first updated-intersected))) (first (:intersections (first updated-intersected))))
        (l/line (first (:intersections (first updated-intersected))) (:wall-point (first (:intersections (first updated-intersected)))))
        (l/line (:wall-point (first (:intersections (first updated-intersected)))) (:wall-point (first (:intersections (second updated-intersected)))))
        (l/line (:wall-point (first (:intersections (second updated-intersected)))) (first (:intersections (second updated-intersected))))
        (l/line (first (:intersections (second updated-intersected))) (:p2 (:line (second updated-intersected)))))
      :else (list))))

(defn visible?
  [polygon-line wall center]
  (and (nil? (l/intersect-segments wall (l/line center (:p1 polygon-line))))
       (nil? (l/intersect-segments wall (l/line center (:p2 polygon-line))))))

(defn find-intersections
  [polygon line]
  (map #(hash-map :line % :intersections (filter (fn [i] (not (nil? i))) (list (l/intersect-segments % line)))) (:lines polygon)))

(defn cut
  "Cut polygon by intersecting with a line segment. Four cases are possible:
  - Line segment doesn't intersect polygon at all -> do nothing
  - Line segment is contained in circle -> get intersections by casting lines from center over segment start and end
                                           to polygon lines, thus introducing three new lines.
  - Line has one intersection -> cast a line to the contained segment endpoint to the polygon lines. Introduces two new lines.
  - Line has two intersections -> introduce one new line."
  [polygon line]
  (let [intersections (find-intersections polygon line)
        updated-intersections (update-intersections polygon line intersections)
        new-lines (cut-new-lines polygon line intersections updated-intersections)
        start (map :line (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) updated-intersections))
        end (map :line (reverse (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) (reverse updated-intersections))))]
    (Polygon2. (concat start new-lines end) (:center polygon))))
