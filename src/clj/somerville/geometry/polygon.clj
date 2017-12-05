(ns somerville.geometry.polygon
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]))

;; Define a polygon as a list of lines and an optional center.
;; This polygon is specifically used for calculating visibility.
(defrecord Polygon2 [lines center]
  c/Printable
  (c/out [this i] (str (c/indent i) "Polygon of lines:\n" (clojure.string/join "\n" (map #(if (nil? %) "NIL" (c/out % (inc i))) lines))))
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
  (case (intersection-count intersections)
    1 (let [in-point (first (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line))))
            shadow-point (assoc (cast-shadow polygon in-point) :wall-point in-point)]
        (map #(if (l/point-on-segment? (:line %) shadow-point) (assoc % :intersections (conj (:intersections %) shadow-point)) %) intersections))
    0 (let [shadow-point-1 (assoc (cast-shadow polygon (:p1 line)) :wall-point (:p1 line))
            shadow-point-2 (assoc (cast-shadow polygon (:p2 line)) :wall-point (:p2 line))]
        (map #(let [result %
                    result (if (l/point-on-segment? (:line result) shadow-point-1) (assoc result :intersections (conj (:intersections result) shadow-point-1)) result)
                    result (if (l/point-on-segment? (:line result) shadow-point-2) (assoc result :intersections (conj (:intersections result) shadow-point-2)) result)]
                result)
             intersections))
    intersections))

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
      (and (= 1 (count intersected)) (= 2 (count updated-intersected)))
      (let [i1 (first (:intersections (first updated-intersected)))
            i2 (first (:intersections (second updated-intersected)))
            wall-point (if (nil? (:wall-point i1)) (:wall-point i2) (:wall-point i1))]
        (list
          (l/line (:p1 (:line (first updated-intersected))) i1)
          (l/line i1 wall-point)
          (l/line wall-point i2)
          (l/line i2 (:p2 (:line (second updated-intersected))))))
      (and (= 1 (count intersected)) (= 1 (count updated-intersected)))
      (let [i1 (second (:intersections (first updated-intersected)))
            i2 (first (:intersections (first updated-intersected)))
            wall-point (if (nil? (:wall-point i1)) (:wall-point i2) (:wall-point i1))]
        (list
          (l/line (:p1 (:line (first updated-intersected))) i1)
          (l/line i1 wall-point)
          (l/line wall-point i2)
          (l/line i2 (:p2 (:line (first updated-intersected))))))
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

(defn cut-old
  "Cut polygon by intersecting with a line segment. Four cases are possible:
  - Line segment doesn't intersect polygon at all -> do nothing
  - Line segment is contained in circle -> get intersections by casting lines from center over segment start and end
                                           to polygon lines, thus introducing three new lines.
  - Line has one intersection -> cast a line to the contained segment endpoint to the polygon lines. Introduces two new lines.
  - Line has two intersections -> introduce one new line."
  [polygon line]
  (let [tmp (dorun (println "====================================================="))
        tmp (dorun (println (c/out polygon)))
        tmp (dorun (println line))
        intersections (find-intersections polygon line)
        updated-intersections (update-intersections polygon line intersections)
        new-lines (cut-new-lines polygon line intersections updated-intersections)
        start (map :line (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) updated-intersections))
        end (map :line (reverse (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) (reverse updated-intersections))))]
    (Polygon2. (concat start new-lines end) (:center polygon))))

(defn do-cut
  "Cut polygon by intersecting with a line segment. Four cases are possible:
  - Line segment is contained in circle -> get intersections by casting lines from center over segment start and end
                                           to polygon lines, thus introducing three new lines.
  - Line has one intersection -> cast a line to the contained segment endpoint to the polygon lines. Introduces two new lines.
  - Line has two intersections -> introduce one new line."
  [polygon line]
  (let [intersections (find-intersections polygon line)
        updated-intersections (update-intersections polygon line intersections)
        tmp (dorun (map #(println (count (:intersections %))) updated-intersections))
        new-lines (cut-new-lines polygon line intersections updated-intersections)
        tmp (dorun (map #(println (c/out %)) new-lines))
        start (map :line (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) updated-intersections))
        tmp (dorun (println (str "Start: " (count start))))
        end (map :line (reverse (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) (reverse updated-intersections))))
        tmp (dorun (println (str "End: " (count end))))
        ]
    (Polygon2. (concat start new-lines end) (:center polygon))))

(defn cut
  "Cut polygon by intersecting with a line segment. Four cases are possible:
  - Line segment doesn't intersect polygon at all -> do nothing
  - Line segment is contained in circle -> get intersections by casting lines from center over segment start and end
                                           to polygon lines, thus introducing three new lines.
  - Line has one intersection -> cast a line to the contained segment endpoint to the polygon lines. Introduces two new lines.
  - Line has two intersections -> introduce one new line."
  [polygon line]
  (let [intersections (count (intersect-segments polygon line))
        not-relevant (and (= 0 intersections) (not (point-inside-polygon? polygon (:p1 line))) (not (point-inside-polygon? polygon (:p2 line))))
        tmp (when-not not-relevant (dorun (println "=====================================================")))
        tmp (when-not not-relevant (dorun (println (str "Intersections: " intersections))))
        tmp (when-not not-relevant (dorun (println (c/out polygon))))
        tmp (when-not not-relevant (dorun (println (c/out line))))]
    (if not-relevant
      polygon
      (do-cut polygon line))))
