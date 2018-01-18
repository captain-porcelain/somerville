(ns somerville.dungeons.discovery
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.polygon :as poly]
    [taoensso.timbre :as log]))

;;==================================================================================================================
;; Parsing wall descriptions

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
;; Defining the events of the discovery process. Each event represents an end or starting point of a wall.
;; The events are sorted by the angle so the processing can sweep through them.
;; Events are grouped if multiple occur at the same angle.

(defrecord Event [angle point wall angle-2]
  gcommons/Printable
  (gcommons/out [this i] (str (gcommons/indent i) "Event at angle " angle " for " (gcommons/out point (inc i)) "\n"
                              "and angle-2 " angle-2 " for " (gcommons/out wall (inc i))))
  (gcommons/out [this] (gcommons/out this 0)))

(defn event
  [angle point wall angle-2]
  (Event. angle point wall angle-2))

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
            #(let [a1 (p/angle-pos point (:p1 %) ref-point)
                   a2 (p/angle-pos point (:p2 %) ref-point)]
               (list
                 (event a1 (:p1 %) % a2)
                 (event a2 (:p2 %) % a1)))
            walls))))))

;;==================================================================================================================
;; Preparation of sight blocking walls. Consider only those lines that are intersecting the circle given
;; by the current point of discovery.

(defn sort-line-points
  "Reorder the two points of a line so they are sorted by the angle defined by the line point, point and ref-point."
  [line point ref-point]
  (let [points (list (:p1 line) (:p2 line))
        angles (sort (map #(p/angle-pos point % ref-point) points))
        sorted (sort-by #(p/angle-pos point % ref-point) points)]
    (cond
      (and (gcommons/close-to 0 (first angles)) (< Math/PI (second angles))) (l/line (second sorted) (first sorted))
      (and (< (first angles) (/ Math/PI 2)) (> (second angles) (/ (* Math/PI 3) 2))) (l/line (second sorted) (first sorted))
      :else (l/line (first sorted) (second sorted)))))

(defn shorten-walls
  "Reduce the walls to those that are inside the polygon."
  [polygon walls]
  (filter #(not (nil? %)) (map #(poly/shorten-line polygon %) walls)))

(defn cut-wall
  "Cut one wall into separate lines where intersected by other walls."
  [wall walls]
  (let [others (filter #(not (= wall %)) walls)
        intersections (sort-by #(p/distance (:p1 wall) %) (l/cuts-segments wall others))
        non-ends (filter #(and (not (= (:p1 wall) %)) (not (= (:p2 wall) %))) intersections)
        points (concat (list (:p1 wall)) non-ends (list (:p2 wall)))]
    (map #(l/line %1 %2) points (rest points))))

(defn cut-walls
  "Cut all walls on intersections."
  [walls]
  (reduce concat (map #(cut-wall % walls) walls)))

(defn relevant-walls
  "Get all walls relevant to the discovered point including limiting
  the distance through polygon approximation of the view circle."
  [point walls visualrange polygon-steps]
  (let [circle-points (c/circle-points (c/circle point visualrange) polygon-steps)
        polygon (poly/from-points circle-points point)
        relevant-walls (shorten-walls polygon (filter #(not (= (:x point) (:x (:p1 %)) (:x (:p2 %)))) walls))
        cuts (cut-walls relevant-walls)]
    (concat (:lines polygon) cuts)))

;;==================================================================================================================
;; Processing the events

;;------------------------------------------------------------------------------------------------------------------
;; Decision Helpers

(defn short?
  "Check if a wall is shorter than 10."
  [wall]
  (< (p/distance (:p1 wall) (:p2 wall)) 10))

(defn relevant-event
  "Given the center point of the discovery and the events at a given angle return the relevant event."
  [point events]
  (first (sort-by #(p/distance point (:point %)) events)))

(defn relevant-wall
  "Get the closest wall to point of discovery."
  [point relevant-point walls]
  (let [line (l/line point relevant-point)]
    (first (sort-by #(p/distance point (if (l/parallel? line %)
                                         (first (sort-by (fn [lp] (p/distance point lp)) (list (:p1 %) (:p2 %))))
                                         (l/intersect line %))) walls))))

(defn consider-event?
  "Check if an event is visible."
  [point event walls]
  (let [line (l/line point (:point event))
        dist (p/distance point (:point event))
        intersections (filter #(not (nil? %)) (map #(l/intersect-segments line %) walls))
        closer (filter #(and (not (gcommons/close-to dist (p/distance point %))) (< (p/distance point %) dist)) intersections)]
    (= 0 (count closer))))

(defn cast-point
  "Find the next triangle point either at the event or at the wall behind the event."
  [point event walls]
  (let [other-walls (filter #(not (or (= (:point event) (:p1 %)) (= (:point event) (:p2 %)))) walls)
        ws (if (= 0 (count other-walls)) walls other-walls)]
    (first (sort-by #(p/distance point %) (l/cuts (l/line point (:point event)) ws)))))

(defn active-walls
  "Get the list of active walls based on the events at an angle."
  [current-walls events]
  (let [ps (map :point events)]
    (concat
      (filter #(not (commons/in? (:p2 %) ps)) current-walls)
      (distinct (filter #(commons/in? (:p1 %) ps) (map :wall events))))))

(defn event-type
  "Check which type of event this is."
  [point event walls]
  (let [starting (count (filter #(= (:point event) (:p1 %)) walls))
        ending (count (filter #(= (:point event) (:p2 %)) walls))]
    (cond
      (and (< 0 starting) (< 0 ending)) :contact
      (and (< 0 starting) (not (< 0 ending))) :starting
      (and (not (< 0 starting)) (< 0 ending)) :ending
      :else :bonkers)))

;;------------------------------------------------------------------------------------------------------------------
;; Actual Discovery Process

(defn update-triangles
  "This is the function where most of the logic sits. It represents the processing of the events at one angle.
  It finds next point of a triangle, and updates relevant walls and adds triangles."
  [point last-point current-triangles current-walls last-walls current-events remaining]
  (let [event (relevant-event point current-events)
        new-walls (active-walls current-walls current-events)
        etype (event-type point event (concat current-walls last-walls new-walls))]
    (cond
      (or (nil? last-point) (= 0 (count current-walls)))
        [(:point event) current-triangles new-walls]
      (or (consider-event? point event current-walls) (= 0 remaining))
        (let [p (cast-point point event current-walls)
              triangle-point (if (and (< 0 remaining) (or (= :contact etype) (= :ending etype))) (:point event) p)
              new-point (if (or (= :contact etype) (= :starting etype)) (:point event) p)
              triangle (t/triangle point last-point triangle-point)]
          [new-point (conj current-triangles triangle) new-walls])
      :else
        [last-point current-triangles new-walls])))

(defn visible-triangles
  "Find the visible triangles."
  [point events debugmapref]
  (let [event-line (l/line point (:point (relevant-event point (first events))))
        all-walls (map :wall (reduce concat events))
        i (first (sort-by #(p/distance point %) (l/cuts-segments event-line all-walls)))
        intersected-walls (filter #(l/point-on-segment? % i) all-walls)
        starting-walls (if (nil? i) (list) intersected-walls)]
    (loop [remaining events
           last-point i
           walls starting-walls
           last-walls (list)
           triangles (list)]
      (if (= 0 (count remaining))
        triangles
        (let [[new-point new-triangles new-walls] (update-triangles point last-point triangles walls last-walls (first remaining) (count (rest remaining)))
              de {:remaining remaining :point point :new-walls new-walls :walls walls :last-walls last-walls :new-point new-point :new-triangles new-triangles :relevant (relevant-event point (first remaining))}
              tmp (when-not (nil? debugmapref) (swap! debugmapref #(assoc % :steps (conj (:steps %) de))))]
          (recur
            (rest remaining)
            new-point
            new-walls
            walls
            new-triangles))))))

(defn discover-point
  "Discover the visible area created by one discovered point."
  ([point wall-description visualrange debugmapref]
   (let [polygon-steps 16
         walls (parse wall-description)
         ref-point (p/point -1 (:y point))
         sorted-walls (map #(sort-line-points % point ref-point) (filter #(not (short? %)) (relevant-walls point walls visualrange polygon-steps)))
         events (gather-events sorted-walls point ref-point)
         events (concat events (list (first events)))
         tmp (when-not (nil? debugmapref) (swap! debugmapref #(assoc % :polygon-steps polygon-steps
                                                                     :point point
                                                                     :ref-point ref-point
                                                                     :sorted-walls sorted-walls
                                                                     :visualrange visualrange
                                                                     :events events
                                                                     :steps [])))
         triangles (visible-triangles point events debugmapref)]
     triangles))
  ([point wall-description visualrange]
   (discover-point point wall-description visualrange nil)))
