;; Geometric discovery to create overlay images that only show parts of an image that have been discovered.
;; The discovery is based on ray casting to wall points.
;; See https://www.redblobgames.com/articles/visibility/
(ns somerville.dungeons.discovery
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon BasicStroke RenderingHints]
    [java.awt.image BufferedImage])
  (:require
    [somerville.commons :as commons]
    [somerville.image :as image]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.polygon :as poly]
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
;; Drawing helpers

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

(defn setup-graphics
  "Prepare the Java Graphics object for rendering."
  [image]
  (let [graphics ^Graphics2D (.createGraphics image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (.setStroke graphics (BasicStroke. 2))
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)]
  graphics))

(defn draw-triangle
  "Transform a triangle into a Java graphics polygon and render it."
  [triangle graphics]
  (let [xs (into-array Integer/TYPE (list (:x (:p1 triangle)) (:x (:p2 triangle)) (:x (:p3 triangle))))
        ys (into-array Integer/TYPE (list (:y (:p1 triangle)) (:y (:p2 triangle)) (:y (:p3 triangle))))
        p (Polygon. xs ys (count xs))
        tmp (.fillPolygon graphics p)
        tmp (.drawPolygon graphics p)]))

;;==================================================================================================================
;; Debugging helpers

(def debug (atom false))
(def debug-fn-1 (atom nil))
(def debug-fn (atom nil))
(def debug-point (atom 0))
(def debug-step (atom 0))

(defn debug-draw
  "Create an image of size width x height with transparency and paint it completely black."
  [^String filename ^Integer width ^Integer height circle points lines active-walls last-point triangles]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics Color/white)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.setPaint graphics Color/black)
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) lines))
        tmp (.setPaint graphics Color/red)
        tmp (.drawOval graphics (- (:x (:p circle)) (:r circle)) (- (:y (:p circle)) (:r circle)) (* 2 (:r circle)) (* 2 (:r circle)))
        tmp (.drawLine graphics (- (:x (:p circle)) 5) (- (:y (:p circle)) 5) (+ (:x (:p circle)) 5) (+ (:y (:p circle)) 5))
        tmp (.drawLine graphics (- (:x (:p circle)) 5) (+ (:y (:p circle)) 5) (+ (:x (:p circle)) 5) (- (:y (:p circle)) 5))
        tmp (.setPaint graphics Color/green)
        tmp (dorun (map #(do
                           (.drawLine graphics (- (:x %) 5) (- (:y %) 5) (+ (:x %) 5) (+ (:y %) 5))
                           (.drawLine graphics (- (:x %) 5) (+ (:y %) 5) (+ (:x %) 5) (- (:y %) 5))) points))
        tmp (.setPaint graphics Color/blue)
        tmp (when-not (nil? last-point)
              (.drawLine graphics (:x last-point) (- (:y last-point) 5) (:x last-point) (+ (:y last-point) 5))
              (.drawLine graphics (- (:x last-point) 5) (:y last-point) (+ (:x last-point) 5) (:y last-point)))
        tmp (.setPaint graphics Color/orange)
        tmp (dorun (map #(.drawLine graphics (:x (:p1 %)) (:y (:p1 %)) (:x (:p2 %)) (:y (:p2 %))) active-walls))
        tmp (.setPaint graphics Color/gray)
        tmp (dorun (map #(draw-triangle % graphics) triangles))
        tmp (.dispose graphics)]
    (image/write-image filename img)))

(defn make-debug-fn-1
  [width height visualrange]
  (fn
    [filename point points sorted-walls active-walls last-point triangles]
    (debug-draw filename width height (c/circle point visualrange) points sorted-walls active-walls last-point triangles)))

(defn make-debug-fn-2
  [point sorted-walls]
  (swap! debug-point inc)
  (reset! debug-step 0)
  (fn
    [filename points active-walls last-point triangles]
    (@debug-fn-1 filename point points sorted-walls active-walls last-point triangles)))

(defn debug-visible-triangles
  [remaining point new-walls current-walls last-walls new-point new-triangles relevant-event]
  (let [ref-point (p/point -1 (:y point))
        adjacent-walls (filter #(or (= (:point relevant-event) (:p1 %)) (= (:point relevant-event) (:p2 %))) (concat current-walls last-walls))
        angles (reduce concat (map #(list (p/angle-pos point (:p1 %) ref-point) (p/angle-pos point (:p2 %) ref-point)) adjacent-walls))
        smaller (and (< 0 (count angles)) (every? #(or (< % (:angle relevant-event)) (gcommons/close-to (:angle relevant-event) %)) angles))
        bigger  (and (< 0 (count angles)) (every? #(or (> % (:angle relevant-event)) (gcommons/close-to (:angle relevant-event) %)) angles))
        starting-walls (filter #(= (:point relevant-event) (:p1 %)) (concat current-walls last-walls new-walls))
        ending-walls (filter #(= (:point relevant-event) (:p2 %)) (concat current-walls last-walls new-walls))
        starting-count (count starting-walls)
        ending-count (count ending-walls)
        other-walls (filter #(not (or (= (:point relevant-event) (:p1 %)) (= (:point relevant-event) (:p2 %)))) current-walls)
        ws (if (= 0 (count other-walls)) current-walls other-walls)
        cp (first (sort-by #(p/distance point %) (l/cuts (l/line point (:point relevant-event)) ws)))
        ]
    (str
      "========================================\nEVENTS\n"
      (clojure.string/join "\n" (map #(gcommons/out %) (first remaining)))
      "\n----------------------------------------\nRELEVANT: " (gcommons/out relevant-event)
      "\n----------------------------------------\nNEW WALLS\n"
      (clojure.string/join "\n" (map #(gcommons/out %) new-walls))
      "\n----------------------------------------\nLAST WALLS\n"
      (clojure.string/join "\n" (map #(gcommons/out %) last-walls))
      "\n----------------------------------------\nCURRENT WALLS\n"
      (clojure.string/join "\n" (map #(gcommons/out %) current-walls))
      "\n----------------------------------------\nOTHER WALLS\n"
      (clojure.string/join "\n" (map #(gcommons/out %) other-walls))
      "\n----------------------------------------\nADJACENT WALLS\n"
      (clojure.string/join "\n" (map #(gcommons/out %) adjacent-walls))
      "\n----------------------------------------\nANGLES\n"
      (clojure.string/join "\n" angles)
      "\n----------------------------------------\nSMALLER, BIGGER: " smaller " - " bigger
      "\n----------------------------------------\nSTARTING, ENDING: " starting-count " - " ending-count
      "\n----------------------------------------\nTRIANGLES\n"
      (clojure.string/join "\n" (map #(gcommons/out %) new-triangles))
      "\n----------------------------------------\nCAST POINT\n" (gcommons/out cp)
      "\n----------------------------------------\nNEW POINT\n" (gcommons/out new-point))))

(defn debug-out
  [remaining point new-walls current-walls last-walls new-point new-triangles event]
  (let [filename (str "/tmp/discovery-point-" @debug-point "-step-" @debug-step)
        tmp (swap! debug-step inc)
        tmp (spit (str filename ".log") (debug-visible-triangles remaining point new-walls current-walls last-walls new-point new-triangles event))
        tmp (@debug-fn (str filename ".png") (map :point (first remaining)) new-walls new-point new-triangles)]))

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

(defn all-lines-upcoming?
  "Check if all walls adjacent to the relevant event are still coming up in the processing."
  [event current-events]
  (let [;adjacent-walls (filter #(or (= (:point event) (:p1 (:wall %))) (= (:point event) (:p2 (:wall %)))) current-events)
        ;angles (reduce concat (map #(list (:angle %) (:angle-2 %)) adjacent-walls))
        angles (reduce concat (map #(list (:angle %) (:angle-2 %)) current-events))
        smaller (every? #(or (< % (:angle event)) (gcommons/close-to (:angle event) %)) angles)
        bigger  (every? #(or (> % (:angle event)) (gcommons/close-to (:angle event) %)) angles)]
    [smaller bigger]))

(defn wall-angles?
  "Check if all walls adjacent to the relevant event are still coming up in the processing."
  [point event walls]
  (let [ref-point (p/point -1 (:y point))
        adjacent-walls (filter #(or (= (:point event) (:p1 %)) (= (:point event) (:p2 %))) walls)
        angles (reduce concat (map #(list (p/angle-pos point (:p1 %) ref-point) (p/angle-pos point (:p2 %) ref-point)) adjacent-walls))
        smaller (and (< 0 (count angles)) (every? #(or (< % (:angle event)) (gcommons/close-to (:angle event) %)) angles))
        bigger  (and (< 0 (count angles)) (every? #(or (> % (:angle event)) (gcommons/close-to (:angle event) %)) angles))]
    [smaller bigger]))

(defn event-type
  "Check which type of event this is."
  [point event walls]
  (let [starting-walls (filter #(= (:point event) (:p1 %)) walls)
        ending-walls (filter #(= (:point event) (:p2 %)) walls)
        starting-count (count starting-walls)
        ending-count (count ending-walls)]
    (cond
      (and (< 0 starting-count) (< 0 ending-count)) :contact
      (and (< 0 starting-count) (not (< 0 ending-count))) :starting
      (and (not (< 0 starting-count)) (< 0 ending-count)) :ending
      :else :bonkers)))

;;------------------------------------------------------------------------------------------------------------------
;; Actual Discovery Process

(defn update-triangles
  "This is the function where most of the logic sits. It represents the processing of the events at one angle.
  It finds next point of a triangle, and updates relevant walls and adds triangles."
  [point last-point current-triangles current-walls last-walls current-events remaining]
  (let [event (relevant-event point current-events)
        wall (relevant-wall point (:point event) current-walls)
        new-walls (active-walls current-walls current-events)
        ;[smaller bigger] (all-lines-upcoming? event current-events)
        ;[smaller bigger] (wall-angles? point event (concat current-walls last-walls))
        etype (event-type point event (concat current-walls last-walls new-walls))]
    (cond
      (or (nil? last-point) (= 0 (count current-walls)))
        [(:point event) current-triangles new-walls]
      (or (consider-event? point event current-walls) (= 0 remaining))
        (let [p (cast-point point event current-walls)
              ;triangle-point (if (or smaller (not bigger)) (:point event) p)
              ;new-point (if smaller p (:point event))
              triangle-point (if (or (= :contact etype) (= :ending etype)) (:point event) p)
              new-point (if (or (= :contact etype) (= :starting etype)) (:point event) p)
              triangle (t/triangle point last-point triangle-point)]
          [new-point (conj current-triangles triangle) new-walls])
      :else
        [last-point current-triangles new-walls])))

(defn visible-triangles
  "Find the visible triangles."
  [point events]
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
              tmp (when @debug (debug-out remaining point new-walls walls last-walls new-point new-triangles (relevant-event point (first remaining))))]
          (recur
            (rest remaining)
            new-point
            new-walls
            walls
            new-triangles))))))

(defn discover-point
  "Discover the visible area created by one discovered point."
  [point walls visualrange graphics]
  (let [polygon-steps 16
        ref-point (p/point -1 (:y point))
        sorted-walls (map #(sort-line-points % point ref-point) (filter #(not (short? %)) (relevant-walls point walls visualrange polygon-steps)))
        events (gather-events sorted-walls point ref-point)
        events (concat events (list (first events)))
        tmp (when-not (nil? @debug-fn-1) (reset! debug-fn (make-debug-fn-2 point sorted-walls)))
        triangles (visible-triangles point events)
        tmp (dorun (map #(draw-triangle % graphics) triangles))]))

(defn discover-all-points
  "Discover the visible areas based on ray casting with wall tracing."
  [points wall-lines ^BufferedImage graphics visualrange]
  (dorun (map #(discover-point % wall-lines visualrange graphics) (map #(p/point (nth % 0) (nth % 1)) points))))


;;==================================================================================================================
;; Discovery Interface
;;

(defn discover
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  [points wall-description width height visualrange]
  (let [discovered-image (create-undiscovered-graphics width height)
        graphics ^Graphics2D (setup-graphics discovered-image)
        wall-lines (parse wall-description)
        tmp (when @debug (reset! debug-fn-1 (make-debug-fn-1 width height visualrange)))
        tmp (do (discover-all-points points wall-lines graphics visualrange))
        tmp (.dispose graphics)]
    discovered-image))
