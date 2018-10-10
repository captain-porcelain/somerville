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
  (delaunay-triangle (triangle/triangle (p/point 0 0) (p/point 0 (* 2 (max-val points :y))) (p/point (* 2 (max-val points :x)) 0))))

(defn invalidates?
  "Check if point invalidates triangle."
  [t p]
  (circle/point-in? (:c t) p))

(defn to-lines
  "Create a map from the lines that make up a triangle, setting value to count, i.e. 1."
  [t]
  (hash-map
    (l/sorted-line (:p1 (:t t)) (:p2 (:t t))) 1
    (l/sorted-line (:p2 (:t t)) (:p3 (:t t))) 1
    (l/sorted-line (:p3 (:t t)) (:p1 (:t t))) 1))

(defn hole
  "Create the hole left be invalidated triangles."
  [triangles]
  (map key (filter #(= 1 (val %)) (reduce + (map to-lines triangles)))))

(defn triangulate-hole
  "Create a list of triangles that fills hole."
  [hole p]
  (map #(delaunay-triangle (triangle/triangle (:p1 %) (:p2 %) p)) hole))

(defn add-point
  "Add a new point to the triangulation."
  [triangles p]
  (let [classified (map (fn [t] [(invalidates? t p) t]) triangles)
        nok (map second (filter first classified))
        ok (map second (filter #(not (first %)) classified))]
    (concat ok (triangulate-hole (hole nok) p))))

(defn delaunay
  "Create Delaunay triangulation."
  [points]
  (loop [bt (list (bounding-triangle points))
         ps points]
    (if (= 0 (count ps))
      bt
      (recur (add-point bt (first ps)) (rest ps)))))

