;; Create Delaunay triangulation.
;; See https://leatherbee.org/index.php/2018/10/06/terrain-generation-3-voronoi-diagrams/
;; And http://paulbourke.net/papers/triangulate/
(ns somerville.geometry.delaunay
  (:require
    [somerville.geometry.commons :as sgc]
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.circle :as circle]
    [somerville.geometry.triangle :as triangle]))


(defrecord Delaunay-triangle [t c]
  sgc/Printable
  (sgc/out [this i] (str (sgc/indent i) "Delaunay triangle\n" (sgc/out t) "\nwith circumcircle\n" (sgc/out c)))
  (sgc/out [this] (sgc/out this 0)))

(defrecord Delaunay-triangulation [points triangles bounds])

(defn delaunay-triangle
  ([t c]
   (Delaunay-triangle. t c))
  ([t]
   (Delaunay-triangle. t (triangle/circumcircle t))))

(defn shares-edge
  "Test if two delaunay triangles share at least one edge."
  [t1 t2]
  (let [ps1 #{(:p1 (:t t1)) (:p2 (:t t1)) (:p3 (:t t1))}
        ps2 #{(:p1 (:t t2)) (:p2 (:t t2)) (:p3 (:t t2))}]
    (pos? (count (clojure.set/intersection ps1 ps2)))))

(defn max-val
  "Get max k value of given points."
  [points k]
  (apply max (map k points)))

(defn bounding-triangle
  "Find a triangle that contains all points."
  [points]
  (delaunay-triangle
    (triangle/triangle
      (p/point 0 0)
      (p/point 0 (* 2 (+ 1 (max-val points :y))))
      (p/point (* 2 (+ 1 (max-val points :x))) 0))))

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
  "Create the hole left be invalidated triangles."
  [triangles]
  (map key (filter #(= 1 (val %)) (apply merge-with + (map to-counted-lines triangles)))))

(defn triangulate-hole
  "Create a list of triangles that fills hole."
  [hole p]
  (map #(delaunay-triangle (triangle/triangle (:p1 %) (:p2 %) p)) hole))

(defn add-point
  "Add a new point to the triangulation."
  [triangulation p]
  (let [triangles (:triangles triangulation)
        classified (map (fn [t] [(invalidates? t p) t]) triangles)
        ;tmp (dorun (println (str "adding " (sgc/out p))))
        nok (map second (filter first classified))
        ;tmp (dorun (println (str "invalidated: " (count nok))))
        ok (map second (filter #(not (first %)) classified))
        ;tmp (dorun (println (str "not invalidated: " (count ok))))
        h (hole nok)
        ;tmp (dorun (println (str "hole: " (clojure.string/join "\n" (map sgc/out h)))))
        th (triangulate-hole h p)
        ;tmp (dorun (println (str "triangulation: " (clojure.string/join "\n" (map sgc/out th)))))
        ]
    (Delaunay-triangulation. (cons (:points triangulation) p) (concat ok th) (:bounds triangulation))))

(defn delaunay
  "Create Delaunay triangulation."
  ([points bounds]
   (if (= 0 (count points))
     (Delaunay-triangulation. (list) (list bounds) bounds)
     (loop [bt (Delaunay-triangulation. (list) (list bounds) bounds)
            ps points]
       (if (= 0 (count ps))
         bt
         (recur (add-point bt (first ps)) (rest ps))))))
  ([points]
   (delaunay points (bounding-triangle points))))

(defn remove-bounds
  [triangulation]
  (filter #(not (shares-edge % (:bounds triangulation))) (:triangles triangulation)))

(defn to-lines
  "Create a map from the lines that make up a triangle, setting value to keep pointer to triangle."
  [t]
  (hash-map
    (l/sorted-line (:p1 (:t t)) (:p2 (:t t))) [t]
    (l/sorted-line (:p2 (:t t)) (:p3 (:t t))) [t]
    (l/sorted-line (:p3 (:t t)) (:p1 (:t t))) [t]))

(defn all-to-lines
  "Convert all triangles to list of unique edges that link to the triangles they touch."
  [triangles]
  (apply merge-with concat (map to-lines triangles)))

(defn center-to-center-line
  "Create a line between the center points on both sides of a delaunay triangle line."
  [tl]
  (l/line (:p (:c (first (val tl)))) (:p (:c (second (val tl))))))

(defn center-to-border-line
  "Create a line from the center on one side of the delaunay triangle line to the border."
  [tl]
  (let [cp (:p (:c (first (val tl))))
        mp (p/midpoint (:p1 (key tl)) (:p2 (key tl)))
        sf (if (triangle/inside? (:t (first (val tl))) cp) 1000 -1000)
        op (p/add (p/scale (p/subtract mp cp) sf) cp)]
    (l/line cp op)))

(defn voronoi-line
  "Create an appropriate line for a voronoi cell."
  [tl]
  (case (count (val tl))
    1 (center-to-border-line tl)
    2 (center-to-center-line tl)))

(defn voronoi
  "Convert Delaunay triangles to voronoi diagram."
  [triangulation]
  (map voronoi-line (all-to-lines (remove-bounds triangulation))))
