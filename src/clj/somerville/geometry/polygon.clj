(ns somerville.geometry.polygon
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [taoensso.timbre :as log]))

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

;;====================================================================================================================================================
;; General Helpers

(defn intersect
  "Intersect a polygon with a line."
  [polygon line]
  (filter #(not (nil? %)) (map #(l/intersect % line) (:lines polygon))))

(defn intersect-segments
  "Intersect a polygon with a line."
  [polygon line]
  (filter #(not (nil? %)) (map #(l/intersect-segments % line) (:lines polygon))))

(defn on-polygon?
  "Check if a point is on some line of a polygon."
  [polygon point]
  (< 0 (count (filter #(l/point-on-segment? % point) (:lines polygon)))))

(defn point-inside-polygon?
  "Check if point is inside the polygon."
  [polygon point]
  (and
    ;(not (on-polygon? polygon point))
    (= 0 (count (intersect-segments polygon (l/line (:center polygon) point))))))

(defn shorten-line
  "Reduce line to the parts that are inside the given polygon. If the line is not inside the polygon return nil."
  [polygon line]
  (let [intersections (intersect-segments polygon line)]
    (dorun (map #(println (c/out %)) intersections))
    (case (count intersections)
      0 (if (and (point-inside-polygon? polygon (:p1 line)) (point-inside-polygon? polygon (:p2 line))) line nil)
      1 (l/line (first (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line)))) (first intersections))
      2 (l/line (nth intersections 0) (nth intersections 1))
      line)))

;;====================================================================================================================================================
;; Polygon based visibility checking

;;---------------------------------------------------------------------------------------------------------
;; Finding intersections between wall and polygon

(defn shadow-point
  "Find the point on the polygon that is shadowed by a wall by casting a ray from the center through the endpoint of the wall that is inside the polygon."
  [polygon point]
  (let [ray (l/line (:center polygon) point)
        candidates (intersect polygon ray)
        intersections (filter #(on-polygon? polygon %) candidates)]
    (first (sort-by #(p/distance point %) intersections))))

(defn intersection-count
  "Count the total of intersections in the given intersections-map."
  [intersections]
  ;(reduce + (map #(count (:intersections %)) intersections)))
  (count (into #{} (reduce concat (map :intersections intersections)))))

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

(defn find-intersections
  "Find intersections between polygon lines and given line. Associate each line and the intersections in a map."
  [polygon line]
  (map #(hash-map :line % :intersections (filter (fn [i] (not (nil? i))) (list (l/intersect-segments % line)))) (:lines polygon)))

(defn not-intersected-point
  "Given an line that is intersected in one of it's points get the other point."
  [intersected]
  (if (= (first (:intersections intersected)) (:p1 (:line intersected))) (:p2 (:line intersected)) (:p1 (:line intersected))))

(defn angle
  [center shadow-point point]
  (let [a (p/angle center shadow-point point)]
    (cond
      (c/close-to (* 2 Math/PI) a) (* 2 Math/PI)
      (< Math/PI a) (- (* 2 Math/PI) a)
      (c/close-to 0 a) (* 2 Math/PI)
      :else a)))

(defn sort-intersections
  "Given two intersected lines select the one with the lower angle to the shadow point."
  [intersected shadow-point center]
  (sort-by #(Math/abs (angle center shadow-point (not-intersected-point %))) intersected))

(defn find-intersection-with-shadow
  [polygon line]
  (let [in-point (first (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line))))
        shadow-point (assoc (shadow-point polygon in-point) :wall-point in-point)
        ;;TODO selecting first may cause issues
        shadow-line (first (filter #(l/point-on-segment? % shadow-point) (:lines polygon)))
        candidates (filter #(< 0 (count (:intersections %))) (find-intersections polygon line))
        plausible (sort-intersections candidates (first (:intersections (first candidates))) (:center polygon))
        tmp (log/info (str "shadow-point " (c/out shadow-point)))
        ;tmp (log/info "plausible")
        ;tmp (map log/info plausible)
        ;tmp (log/info ">>>>>>>>>>>>>>")
        ;tmp (map log/info (map #(Math/abs (angle (:center polygon) (first (:intersections (first candidates))) (not-intersected-point %))) plausible))
        ;tmp (log/info ">>>>>>>>>>>>>>")
        intersected (first (if (= 1 (count candidates)) candidates plausible))]
    (map #(cond
            (= % (:line intersected) shadow-line) (assoc intersected :intersections (conj (:intersections intersected) shadow-point))
            (= % (:line intersected)) intersected
            (= % shadow-line) {:line % :intersections (list shadow-point)}
            :else {:line % :intersection (list)})
         (:lines polygon))))

(defn find-shadows
  [polygon line]
  (let [shadow-point-1 (assoc (shadow-point polygon (:p1 line)) :wall-point (:p1 line))
        shadow-point-2 (assoc (shadow-point polygon (:p2 line)) :wall-point (:p2 line))]
    (map #(let [result {:line % :intersections (list)}
                result (if (l/point-on-segment? % shadow-point-1) (assoc result :intersections (conj (:intersections result) shadow-point-1)) result)
                result (if (l/point-on-segment? % shadow-point-2) (assoc result :intersections (conj (:intersections result) shadow-point-2)) result)]
            result)
         (:lines polygon))))

(defn shadow-intersections
  [polygon line]
  (let [in-points (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line)))]
    (case (count in-points)
      2 (find-shadows polygon line)
      1 (find-intersection-with-shadow polygon line)
      0 (find-intersections polygon line)
      :else (map #(hash-map :line % :intersections (list)) (:lines polygon)))))

;;---------------------------------------------------------------------------------------------------------
;; Cutting the polygon

(defn reverse?
  "Check if intersections should cause reversal of new lines."
  [i1 i2]
  (let [q1 (p/quadrant i1)
        q2 (p/quadrant i2)
        iqs [(min q1 q2) (max q1 q2)]
        r (list [1 2] [1 3] [2 2] [2 3] [3 3])]
    (commons/in? iqs r)))

(defn select-visible-point
  "Select the point of a polygon line that is not blocked from view by a wall."
  [line wall center]
  (let [ps (list (:p1 line) (:p2 line))
        candidates (filter #(point-visible? % wall center) ps)]
    (first candidates)))

(defn new-lines-two-intersections
  [intersected wall center]
  (let [p1 (select-visible-point (:line (first intersected)) wall center)
        i1 (first (:intersections (first intersected)))
        i2 (first (:intersections (second intersected)))
        p2 (select-visible-point (:line (second intersected)) wall center)]
    (list p1 i1 i2 p2)))

(defn new-lines-one-intersection-shadow-other
  [intersected wall center]
  (let [p1 (select-visible-point (:line (first intersected)) wall center)
        tmp (log/info (c/out (:line (first intersected))))
        tmp (log/info "p1: " p1)
        i1 (first (:intersections (first intersected)))
        i2 (first (:intersections (second intersected)))
        wall-point (if (nil? (:wall-point i1)) (:wall-point i2) (:wall-point i1))
        p2 (select-visible-point (:line (second intersected)) wall center)]
    (list p1 i1 wall-point i2 p2)))

(defn new-lines-one-intersection-shadow-same
  [intersected wall center]
  (let [p1 (:p1 (:line (first intersected)))
        is (sort-by #(p/distance p1 %) (:intersections (first intersected)))
        i1 (first is)
        i2 (second is)
        wall-point (if (nil? (:wall-point i1)) (:wall-point i2) (:wall-point i1))
        p2 (:p2 (:line (first intersected)))]
    (list p1 i1 wall-point i2 p2)))

(defn new-lines-no-intersections-shadow-same
  [intersected wall center]
  (list
    (:p1 (:line (first intersected)))
    (first (:intersections (first intersected)))
    (:wall-point (first (:intersections (first intersected))))
    (:wall-point (second (:intersections (first intersected))))
    (second (:intersections (first intersected)))
    (:p2 (:line (first intersected)))))

(defn new-lines-no-intersections-shadow-other
  [intersected wall center]
  (list
    (select-visible-point (:line (first intersected)) wall center)
    (first (:intersections (first intersected)))
    (:wall-point (first (:intersections (first intersected))))
    (:wall-point (first (:intersections (second intersected))))
    (first (:intersections (second intersected)))
    (select-visible-point (:line (second intersected)) wall center)))

(defn to-lines
  "Given a set of points create a list of lines connecting them, without closing back from the last to the first point"
  [points reversed]
  (let [ps (if reversed (reverse points) points)]
    (map #(l/line %1 %2) (butlast ps) (rest ps))))

(defn cut-new-lines-2
  "Create a set of lines that represent the affected part of polygon."
  [polygon line intersections]
  (let [in-points (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line)))
        tmp (log/info (str "Checking point " (c/out (:p1 line)) ": " (point-inside-polygon? polygon (:p1 line))))
        tmp (log/info (str "Checking point " (c/out (:p2 line)) ": " (point-inside-polygon? polygon (:p2 line))))
        intersected (filter #(< 0 (count (:intersections %))) intersections)
        tmp (log/info "Creating new lines")
        intersections (reduce concat (map :intersections intersected))
        center (:center polygon)
        reversed (reverse? (first intersections) (second intersections))
        tmp (log/info (str "Case: " (count in-points) " - " (count intersected)))]
    (to-lines
      (cond
        (= 0 (count in-points))                                 (new-lines-two-intersections intersected line center)
        (and (= 1 (count in-points)) (= 2 (count intersected))) (new-lines-one-intersection-shadow-other intersected line center)
        (and (= 1 (count in-points)) (= 1 (count intersected))) (new-lines-one-intersection-shadow-same intersected line center)
        (and (= 2 (count in-points)) (= 1 (count intersected))) (new-lines-no-intersections-shadow-same intersected line center)
        (and (= 2 (count in-points)) (= 2 (count intersected))) (new-lines-no-intersections-shadow-other intersected line center)
        :else (list))
      reversed)))

(defn cut-new-lines
  "Create a set of lines that represent the affected part of polygon."
  [polygon line intersections updated-intersections]
  (let [intersected (filter #(< 0 (count (:intersections %))) intersections)
        tmp (log/info "Creating new lines")
        updated-intersected (filter #(< 0 (count (:intersections %))) updated-intersections)
        intersections (reduce concat (map :intersections updated-intersected))
        center (:center polygon)
        reversed (reverse? (first intersections) (second intersections))
        tmp (log/info (str "Case: " (count intersected) " - " (count updated-intersected)))]
    (to-lines
      (cond
        (= 2 (count intersected))                                         (new-lines-two-intersections intersected line center)
        (and (= 1 (count intersected)) (= 2 (count updated-intersected))) (new-lines-one-intersection-shadow-other updated-intersected line center)
        (and (= 1 (count intersected)) (= 1 (count updated-intersected))) (new-lines-one-intersection-shadow-same updated-intersected line center)
        (and (= 0 (count intersected)) (= 1 (count updated-intersected))) (new-lines-no-intersections-shadow-same updated-intersected line center)
        (and (= 0 (count intersected)) (= 2 (count updated-intersected))) (new-lines-no-intersections-shadow-other updated-intersected line center)
        :else (list))
      reversed)))

(defn unaffected-lines
  [polygon line intersections]
  (let [remaining (drop-while #(or (not (visible? (:line %) line (:center polygon))) (< 0 (count (:intersections %)))) intersections)
        start (map :line (take-while #(and (visible? (:line %) line (:center polygon)) (= 0 (count (:intersections %)))) remaining))
        skipped (< 0 (- (count intersections) (count remaining)))
        end (if skipped (list) (map :line (reverse (take-while #(= 0 (count (:intersections %))) (reverse intersections)))))]
    {:start start :end end :skipped skipped}))

(defn do-cut-2
  "Handle an actual cut of a line with a polygon."
  [polygon line]
  (let [intersections (shadow-intersections polygon line)
        visible-lines (unaffected-lines polygon line intersections)
        new-lines (cut-new-lines-2 polygon line intersections)
        tmp (log/info (str "Count new-lines: " (count new-lines)))
        tmp (map #(log/info (c/out %)) new-lines)]
    (Polygon2. (concat (:start visible-lines) new-lines (:end visible-lines)) (:center polygon))))

(defn do-cut
  "Handle an actual cut of a line with a polygon."
  [polygon line]
  (let [intersections (find-intersections polygon line)
        updated-intersections (update-intersections polygon line intersections)
        visible-lines (unaffected-lines polygon line updated-intersections)
        new-lines (cut-new-lines polygon line intersections updated-intersections)
        tmp (log/info (str "Count new-lines: " (count new-lines)))
        tmp (map #(log/info (c/out %)) new-lines)]
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
        tmp (log/info "=====================================================")
        tmp (log/info "Cutting")
        tmp (log/info (c/out line))
        tmp (log/info (str "Relevant: " (not not-relevant)))
        tmp (when-not not-relevant (log/info (str "Intersections: " intersections)))
        tmp (when-not not-relevant (log/info (c/out polygon)))
        ]
    (if not-relevant
      polygon
      (do-cut-2 polygon line))))

;; Idea for third approach
;; - refactor polygon to list of points
;; - cutting polygon
;;   - check for in-points
;;   - cast shadows
;;   - get list of wall lines (intersections with polygon, in-points, shadow-points)
;;   - iterate through points clock wise
;;     - take point from polygon if not blocked
;;     - take points from wall lines that blocked view
;;
;; General issues remaining
;; what happens when wall has 3 or more intersections?
;; should be possible by keeping intersections, in-points, shadow-points for wall together and picking correctly

