;; Create Delaunay triangulation.
;; See https://leatherbee.org/index.php/2018/10/06/terrain-generation-3-voronoi-diagrams/
;; And http://paulbourke.net/papers/triangulate/
(ns somerville.geometry.delaunay
  (:require
    [somerville.geometry.commons :as sgc]
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.circle :as circle]
    [somerville.geometry.triangle :as triangle]
    [taoensso.timbre :as log]))


(defrecord Delaunay-triangle [t c]
  sgc/Printable
  (sgc/out [this i] (str (sgc/indent i) "Delaunay triangle\n" (sgc/out t) "\nwith circumcircle\n" (sgc/out c)))
  (sgc/out [this] (sgc/out this 0)))

(defrecord Delaunay-triangulation [points triangles bounds max-point])

(defn delaunay-triangle
  ([t c]
   (Delaunay-triangle. t c))
  ([t]
   (Delaunay-triangle. t (triangle/circumcircle t))))

(defn shares-point
  "Test if two delaunay triangles share at least one point."
  [t1 t2]
  (let [ps1 #{(:p1 (:t t1)) (:p2 (:t t1)) (:p3 (:t t1))}
        ps2 #{(:p1 (:t t2)) (:p2 (:t t2)) (:p3 (:t t2))}]
    (pos? (count (clojure.set/intersection ps1 ps2)))))

(defn min-val
  "Get min k value of given points."
  [points k]
  (apply min (map k points)))

(defn max-val
  "Get max k value of given points."
  [points k]
  (apply max (map k points)))

(defn bounding-triangle
  "Find a triangle that contains all points."
  [points]
  (let [maxx (max-val points :x)
        maxy (max-val points :y)
        minx (- (min-val points :x) 1)
        miny (- (min-val points :y) 1)]
    (delaunay-triangle
      (triangle/triangle
        (p/point minx miny)
        (p/point minx (* 2 (+ 1 maxy)))
        (p/point (* 2 (+ 1 maxx)) miny)))))

(defn invalidates?
  "Check if point invalidates triangle."
  [dt p]
  (or (circle/point-in? (:c dt) p) (circle/point-on? (:c dt) p)))

(defn to-counted-lines
  "Create a map from the lines that make up a triangle, setting value to count, i.e. 1."
  [t]
  (hash-map
    (l/sorted-line (:p1 (:t t)) (:p2 (:t t))) 1
    (l/sorted-line (:p2 (:t t)) (:p3 (:t t))) 1
    (l/sorted-line (:p3 (:t t)) (:p1 (:t t))) 1))

(defn hole
  "Create the hole left by invalidated triangles."
  [triangles]
  (map key (filter #(= 1 (val %)) (apply merge-with + (map to-counted-lines triangles)))))

(defn triangulate-hole
  "Create a list of triangles that fills hole."
  [hole p]
  ;(log/info "HOLE")
  ;(log/info (sgc/out p))
  ;(dorun (map #(log/info (sgc/out %)) hole))
  (map #(delaunay-triangle (triangle/triangle (:p1 %) (:p2 %) p)) hole))

(defn add-point
  "Add a new point to the triangulation."
  [triangulation p]
  (let [triangles (:triangles triangulation)
        classified (map (fn [t] [(invalidates? t p) t]) triangles)
        nok (map second (filter first classified))
        ok (map second (filter #(not (first %)) classified))
        th (triangulate-hole (hole nok) p)]
    (Delaunay-triangulation. (conj (:points triangulation) p) (concat ok th) (:bounds triangulation) (:max-point triangulation))))

(defn delaunay
  "Create Delaunay triangulation."
  ([points min-point max-point]
   (let [bounds (bounding-triangle (list min-point max-point))]
     (if (= 0 (count points))
       (Delaunay-triangulation. (list) (list bounds) bounds max-point)
       (loop [bt (Delaunay-triangulation. (list) (list bounds) bounds max-point)
              ps points]
         (if (= 0 (count ps))
           bt
           (recur (add-point bt (first ps)) (rest ps)))))))
  ([points max-point]
   (delaunay points (p/point (min-val points :x) (min-val points :y)) max-point))
  ([points]
   (delaunay points (p/point (min-val points :x) (min-val points :y)) (p/point (max-val points :x) (max-val points :y)))))

(defn remove-bounds
  "Remove the triangles that share a point with the boundary"
  [triangulation]
  (filter #(not (shares-point % (:bounds triangulation))) (:triangles triangulation)))

(defn triangle-lines
  "Create a map from the lines that make up a triangle, setting value to keep pointer to triangle."
  [t]
  (hash-map
    (l/sorted-line (:p1 (:t t)) (:p2 (:t t))) [t]
    (l/sorted-line (:p2 (:t t)) (:p3 (:t t))) [t]
    (l/sorted-line (:p3 (:t t)) (:p1 (:t t))) [t]))

(defn all-to-triangle-lines
  "Convert all triangles to list of unique edges that link to the triangles they touch."
  [triangles]
  (apply merge-with concat (map triangle-lines triangles)))

(defrecord VoronoiLine [line points])

(defn center-to-center-line
  "Create a line between the center points on both sides of a delaunay triangle line."
  [tl]
  (l/line (:p (:c (first (val tl)))) (:p (:c (second (val tl))))))

(defn center-to-border-line
  "Create a line from the center on one side of the delaunay triangle line to the border."
  [ti tb]
  (let [cp (:p (:c ti))
        mp (:p (:c tb))
        op (p/add (p/scale (p/subtract mp cp) 1000) cp)]
    (l/line cp op)))

(defn voronoi-line
  "Create an appropriate line for a voronoi cell."
  [tl bounds]
  (let [inner (filter #(not (shares-point % bounds)) (val tl))
        border (filter #(shares-point % bounds) (val tl))
        ps (list (:p1 (key tl)) (:p2 (key tl)))]
    (cond
      (= 2 (count inner)) (VoronoiLine. (center-to-center-line tl) ps)
      (and (= 1 (count inner)) (= 1 (count border))) (VoronoiLine. (center-to-border-line (first inner) (first border)) ps)
      :else nil)))

(defn voronoi-lines
  "Convert Delaunay triangles to lines of a voronoi diagram."
  [triangulation]
  (filter
    #(not (nil? %))
    (map
      #(voronoi-line % (:bounds triangulation))
      (all-to-triangle-lines (:triangles triangulation)))))

(defn voronoi
  "Convert Delaunay triangles to voronoi diagram."
  [triangulation]
  (voronoi-lines triangulation))

