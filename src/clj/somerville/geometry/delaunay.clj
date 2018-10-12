;; Create Delaunay triangulation.
;; See https://leatherbee.org/index.php/2018/10/06/terrain-generation-3-voronoi-diagrams/
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

(defn delaunay-triangle
  ([t c]
   (Delaunay-triangle. t c))
  ([t]
   (Delaunay-triangle. t (triangle/circumcircle t))))

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
  [triangles p]
  (let [classified (map (fn [t] [(invalidates? t p) t]) triangles)
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
    (concat ok th)))

(defn delaunay
  "Create Delaunay triangulation."
  ([points bounds]
   (if (= 0 (count points))
     (list bounds)
     (loop [bt (list bounds)
            ps points]
       (if (= 0 (count ps))
         bt
         (recur (add-point bt (first ps)) (rest ps))))))
  ([points]
   (delaunay points (bounding-triangle points))))


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

(defn voronoi-line
  [t]
  (l/line (:p (:c (first (val t)))) (:p (:c (second (val t))))))

(defn voronoi
  "Convert Delaunay triangles to voronoi diagram."
  [triangles]
  (let [lines (all-to-lines triangles)]
    (map voronoi-line (filter #(= 2 (count (val %))) lines))))
