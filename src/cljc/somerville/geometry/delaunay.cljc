;; Create Delaunay triangulation.
;; See https://leatherbee.org/index.php/2018/10/06/terrain-generation-3-voronoi-diagrams/
;; And http://paulbourke.net/papers/triangulate/
(ns somerville.geometry.delaunay
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.commons :as sgc]
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.circle :as circle]
    [somerville.geometry.triangle :as triangle]
    [somerville.geometry.polygon :as polygon]
    [clojure.string :as string]
    [taoensso.timbre :as log]))


;;====================================================================================================
;; Creating Delaunay Triangulations

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
  (map #(delaunay-triangle (triangle/triangle (:p1 %) (:p2 %) p)) (map l/sorted-line hole)))

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


;;====================================================================================================
;; Creating Voronoi Lines

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
(defrecord VoronoiCell [point points closed]
  sgc/Printable
  (sgc/out [this i] (str (sgc/indent i) "Voronoi Cell " (if closed "(closed)" "(open)") " for " (sgc/out point i) " with points: " (string/join ", " (map #(sgc/out % i) points))))
  (sgc/out [this] (sgc/out this 0)))

(defn center-to-center-line
  "Create a line between the center points on both sides of a delaunay triangle line."
  [t1 t2 points]
  (VoronoiLine. (l/sorted-line (:p (:c t1)) (:p (:c t2))) points))

(defn center-to-border-line
  "Create a line from the center on one side of the delaunay triangle line to the border."
  [ti tb points]
  (let [cp (:p (:c ti))
        mp (:p (:c tb))
        op (p/add (p/scale (p/subtract mp cp) 1000) cp)]
    (VoronoiLine. (l/sorted-line cp op) points)))

(defn voronoi-line
  "Create an appropriate line for a voronoi cell."
  [tl bounds]
  (let [inner (filter #(not (shares-point % bounds)) (val tl))
        border (filter #(shares-point % bounds) (val tl))
        ps (list (:p1 (key tl)) (:p2 (key tl)))]
    (cond
      (= 2 (count inner)) (center-to-center-line (first inner) (second inner) ps)
      (and (= 1 (count inner)) (= 1 (count border))) (center-to-border-line (first inner) (first border) ps)
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


;;====================================================================================================
;; Creating Voronoi Polygons

(defn lines-of-point
  "Filter the lines to those that relate to given point."
  [lines point]
  (map :line (filter #(commons/in? point (:points %)) lines)))

(defn points-from-lines
  "Extract the voronoi points from the lines again."
  [lines]
  (into #{} (reduce concat (map :points lines))))

(defn collect-points
  "Collect the voronoi points with their relevant lines."
  [lines]
  (map (fn [p] {:point p :lines (lines-of-point lines p)}) (points-from-lines lines)))

(defn next-line
  "Get the next line that builds a cell."
  [points lines]
  (first (filter #(or (commons/in? (:p1 %) points) (commons/in? (:p2 %) points)) lines)))

(defn update-points
  "Add relevant point from line to cell points."
  [points line]
  (into []
        (cond
          (and (= (first points) (:p1 line))) (cons (:p2 line) points)
          (and (= (first points) (:p2 line))) (cons (:p1 line) points)
          (and (= (last  points) (:p1 line))) (conj points (:p2 line))
          (and (= (last  points) (:p2 line))) (conj points (:p1 line)))))

(defn make-clockwise-comparator
  "Create a comparator for sorting points clockwise.
  See https://stackoverflow.com/questions/6989100/sort-points-in-clockwise-order"
  [c]
  (fn [a b]
    (let [dxac (- (:x a) (:x c))
          dxbc (- (:x b) (:x c))
          dyac (- (:y a) (:y c))
          dybc (- (:y b) (:y c))
          det (- (* dxac dybc) (* dxbc dyac))
          d1 (+ (* dxac dxac) (* dyac dyac))
          d2 (+ (* dxbc dxbc) (* dybc dybc))]
      (cond
        (and (>= dxac 0) (< dxbc 0)) true
        (and (< dxac 0)(>= dxbc 0)) false
        (and (= dxac 0)(= dxbc 0) (or (>= dyac 0) (>= dybc 0))) (> (:y a) (:y b))
        (and (= dxac 0)(= dxbc 0) (not (or (>= dyac 0) (>= dybc 0)))) (> (:y b) (:y a))
        (< det 0) true
        (> det 0) false
        :else (> d1 d2)))))

(defn to-cell
  "Convert the lines of one voronoi point to the points of a cell."
  [lines]
  (loop [ls (rest lines)
         points [(:p1 (first lines)) (:p2 (first lines))]]
    (let [nl (next-line points ls)
          remaining (into [] (remove #{nl} ls))]
      (if (nil? nl)
        {:points points :closed (= (first points) (last points))}
        (recur remaining (update-points points nl))))))

(defn to-cells
  "Transform lines to voronoi cells."
  [lines]
  (map
    #(let [cell (to-cell (:lines %))
           ;a (p/angle (:point %) (first (:points cell)) (second (:points cell)))
           ;tmp (log/info (sgc/out (:point %)) "has angle" a)
           ;points (if (neg? a) (reverse (:points cell)) (:points cell))
           points (:points cell)
           ;points (sort (make-clockwise-comparator (:point %)) (:points cell))
           ]
       (VoronoiCell. (:point %) points (:closed cell)))
    (collect-points lines)))

