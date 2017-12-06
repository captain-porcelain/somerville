(ns somerville.geometry.polygon
  (:require
    [somerville.commons :as commons]
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

(defn point-inside-polygon?
  "Check if point is inside the polygon."
  [polygon point]
  (= 0 (count (intersect-segments polygon (l/line (:center polygon) point)))))

;;====================================================================================================================================================
;; Polygon based visibility checking

(defn shadow-point
  "Find the point on the polygon that is shadowed by a wall by casting a ray from the center through the endpoint of the wall that is inside the polygon."
  [polygon point]
  (let [ray (l/line (:center polygon) point)
        intersections (intersect polygon ray)]
    (first (sort-by #(p/distance point %) intersections))))

(defn intersection-count
  "Count the total of intersections in the given intersections-map."
  [intersections]
  (reduce + (map #(count (:intersections %)) intersections)))

(defn point-visible?
  "Check if a point is not blocked by the given wall."
  [point wall center]
  (nil? (l/intersect-segments wall (l/line center point))))

(defn visible?
  "Check if both end points of a polygon line are not blocked by the given wall."
  [polygon-line wall center]
  (and (point-visible? (:p1 polygon-line) wall center) (point-visible? (:p2 polygon-line) wall center)))

(defn update-intersections
  "Given the list of intersections-maps update these in case the line is partially or completely contained in the polygon."
  [polygon line intersections]
  (case (intersection-count intersections)
    1 (let [in-point (first (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line))))
            shadow-point (assoc (shadow-point polygon in-point) :wall-point in-point)]
        (map #(if (l/point-on-segment? (:line %) shadow-point) (assoc % :intersections (conj (:intersections %) shadow-point)) %) intersections))
    0 (let [shadow-point-1 (assoc (shadow-point polygon (:p1 line)) :wall-point (:p1 line))
            shadow-point-2 (assoc (shadow-point polygon (:p2 line)) :wall-point (:p2 line))]
        (map #(let [result %
                    result (if (l/point-on-segment? (:line result) shadow-point-1) (assoc result :intersections (conj (:intersections result) shadow-point-1)) result)
                    result (if (l/point-on-segment? (:line result) shadow-point-2) (assoc result :intersections (conj (:intersections result) shadow-point-2)) result)]
                result)
             intersections))
    intersections))

(defn reverse?
  "Check if intersections should cause reversal of new lines."
  [i1 i2]
  (let [q1 (p/quadrant i1)
        q2 (p/quadrant i2)
        iqs [(min q1 q2) (max q1 q2)]
        r (list [1 2] [1 3] [2 2] [2 3] [3 3])]
    (commons/in? iqs r)))

(defn new-lines-two-intersections
  [intersected wall center]
  (let [l1p1 (:p1 (:line (first intersected)))
        l1p2 (:p2 (:line (first intersected)))
        l2p1 (:p1 (:line (second intersected)))
        l2p2 (:p2 (:line (second intersected)))
        p1 (if (point-visible? l1p1 wall center) l1p1 l1p2)
        i1 (first (:intersections (first intersected)))
        i2 (first (:intersections (second intersected)))
        p2 (if (point-visible? l2p1 wall center) l2p1 l2p2)]
    (if (reverse? i1 i2)
      (list
        (l/line p2 i2)
        (l/line i2 i1)
        (l/line i1 p1))
      (list
        (l/line p1 i1)
        (l/line i1 i2)
        (l/line i2 p2)))))

(defn new-lines-one-intersection-shadow-other
  [intersected]
  (let [p1 (:p1 (:line (first intersected)))
        i1 (first (:intersections (first intersected)))
        i2 (first (:intersections (second intersected)))
        wall-point (if (nil? (:wall-point i1)) (:wall-point i2) (:wall-point i1))
        p2 (:p2 (:line (second intersected)))]
    (list
      (l/line p1 i1)
      (l/line i1 wall-point)
      (l/line wall-point i2)
      (l/line i2 p2))))

(defn new-lines-one-intersection-shadow-same
  [intersected]
  (let [p1 (:p1 (:line (first intersected)))
        i1 (second (:intersections (first intersected)))
        i2 (first (:intersections (first intersected)))
        wall-point (if (nil? (:wall-point i1)) (:wall-point i2) (:wall-point i1))
        p2 (:p2 (:line (first intersected)))]
    (list
      (l/line p1 i1)
      (l/line i1 wall-point)
      (l/line wall-point i2)
      (l/line i2 p2))))

(defn new-lines-no-intersections-shadow-same
  [intersected]
  (let [p1 (:p1 (:line (first intersected)))
        i1 (first (:intersections (first intersected)))
        wall-point-1 (:wall-point (first (:intersections (first intersected))))
        wall-point-2 (:wall-point (second (:intersections (first intersected))))
        i2 (second (:intersections (first intersected)))
        p2 (:p2 (:line (first intersected)))]
    (list
      (l/line p1 i1)
      (l/line i1 wall-point-1)
      (l/line wall-point-1 wall-point-2)
      (l/line wall-point-2 i2)
      (l/line i2 p2))))

(defn new-lines-no-intersections-shadow-other
  [intersected]
  (let [p1 (:p1 (:line (first intersected)))
        i1 (first (:intersections (first intersected)))
        wall-point-1 (:wall-point (first (:intersections (first intersected))))
        wall-point-2 (:wall-point (first (:intersections (second intersected))))
        i2 (first (:intersections (second intersected)))
        p2 (:p2 (:line (second intersected)))]
    (list
      (l/line p1 i1)
      (l/line i1 wall-point-1)
      (l/line wall-point-1 wall-point-2)
      (l/line wall-point-2 i2)
      (l/line i2 p2))))

(defn cut-new-lines
  "Create a set of lines that represent the affected part of polygon."
  [polygon line intersections updated-intersections]
  (let [intersected (filter #(< 0 (count (:intersections %))) intersections)
        updated-intersected (filter #(< 0 (count (:intersections %))) updated-intersections)]
    (cond
      (= 2 (count intersected)) (new-lines-two-intersections intersected line (:center polygon))
      (and (= 1 (count intersected)) (= 2 (count updated-intersected))) (new-lines-one-intersection-shadow-other updated-intersected)
      (and (= 1 (count intersected)) (= 1 (count updated-intersected))) (new-lines-one-intersection-shadow-same updated-intersected)
      (and (= 0 (count intersected)) (= 1 (count updated-intersected))) (new-lines-no-intersections-shadow-same updated-intersected)
      (and (= 0 (count intersected)) (= 2 (count updated-intersected))) (new-lines-no-intersections-shadow-other updated-intersected)
      :else (list))))

(defn find-intersections
  "Find intersections between polygon lines and given line. Associate each line and the intersections in a map."
  [polygon line]
  (map #(hash-map :line % :intersections (filter (fn [i] (not (nil? i))) (list (l/intersect-segments % line)))) (:lines polygon)))

(defn unaffected-lines
  [polygon line intersections]
  (let [remaining (drop-while #(or (not (visible? (:line %) line (:center polygon))) (< 0 (count (:intersections %)))) intersections)
        start (map :line (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) remaining))
        skipped (< 0 (- (count intersections) (count remaining)))
        end (if skipped (list) (map :line (reverse (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) (reverse intersections)))))]
    {:start start :end end :skipped skipped}))

(defn do-cut
  "Handle an actual cut of a line with a polygon."
  [polygon line]
  (let [intersections (find-intersections polygon line)
        updated-intersections (update-intersections polygon line intersections)
        visible-lines (unaffected-lines polygon line updated-intersections)
        new-lines (cut-new-lines polygon line intersections updated-intersections)]
    (Polygon2. (concat (:start visible-lines) new-lines (:end visible-lines)) (:center polygon))))

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
        ;tmp (when-not not-relevant (dorun (println "=====================================================")))
        ;tmp (when-not not-relevant (dorun (println (str "Intersections: " intersections))))
        ;tmp (when-not not-relevant (dorun (println (c/out polygon))))
        ;tmp (when-not not-relevant (dorun (println (c/out line))))
        ]
    (if not-relevant
      polygon
      (do-cut polygon line))))

