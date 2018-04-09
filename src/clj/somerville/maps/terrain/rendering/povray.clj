(ns somerville.maps.terrain.rendering.povray
  (:require
    [clojure.string :as s]
    [somerville.geometry.point :as p]
    [somerville.geometry.polygon :as polygon]
    [somerville.maps.grid :as grid]))

(defn grid-points
  [g]
  (let [size (dec (:width g))]
    (for [y (range size)
          x (range size)]
      [x y])))

(defn triangles
  [g [x y]]
  (let [p1 (p/point x y (grid/get-from g x y))
        p2 (p/point (inc x) y (grid/get-from g (inc x) y))
        p3 (p/point (inc x) (inc y) (grid/get-from g (inc x) (inc y)))
        p4 (p/point x (inc y) (grid/get-from g x (inc y)))
        t1 (polygon/from-points (list p1 p2 p4))
        t2 (polygon/from-points (list p2 p3 p4))]
    [t1 t2]))

(defn triangulation-bad
  [g]
  (loop [p2s (grid-points g)
         p3s (list)]
    (if (= 0 (count p2s))
      p3s
      (recur (rest p2s) (concat p3s (triangles g (first p2s)))))))

(defn triangulation
  [g]
  (let [size (dec (:width g))]
    (for [x (range size)
          y (range size)
          which [:upper :lower]]
      (let [p1 (p/point x y (grid/get-from g x y))
            p2 (p/point (inc x) y (grid/get-from g (inc x) y))
            p3 (p/point (inc x) (inc y) (grid/get-from g (inc x) (inc y)))
            p4 (p/point x (inc y) (grid/get-from g x (inc y)))]
        (if (= :upper which)
          (polygon/from-points (list p1 p2 p4))
          (polygon/from-points (list p2 p3 p4)))))))

(defn camera
  [g config]
  (let [scale 20
        camera (p/point (* -20 (/ (:width g) 2)) (* -20 (/ (:height g) 2)) 100)
        focus  (p/point (/ (:width g) 2) (/ (:height g) 2)  0)]
    (str "camera {\n"
         "  location <" (int (:x camera)) ", " (int (:y camera)) ", " (int (:z camera)) ">\n"
         "  look_at <" (int (:x focus)) ", " (int (:y focus)) ", " (int (:z focus)) ">\n"
         "}")))

(defn light_source
  [g config]
  "light_source { <50, 50, -50> color rgb<1, 1, 1> }")

(defn texture
  [g config]
  (str "    texture {\n"
       "      pigment { color rgb<0.9, 0.9, 0.9> }\n"
       "      finish { ambient 0.2 diffuse 0.7 }\n"
       "    }"))

(defn declare-red
  []
  (str "#declare Red = texture {\n"
       "  pigment { color rgb<0.8, 0.2, 0.2> }\n"
       "  finish { ambient 0.2 diffuse 0.5 }\n"
       "}"))

(defn triangle
  [polygon g config]
  (let [ps (polygon/to-points polygon)]
    (str "  triangle {\n"
         "    <" (float (:x (nth ps 0))) "," (float (:y (nth ps 0))) "," (float (:z (nth ps 0))) ">"
         ",\n"
         "    <" (float (:x (nth ps 1))) "," (float (:y (nth ps 1))) "," (float (:z (nth ps 1))) ">"
         ",\n"
         "    <" (float (:x (nth ps 2))) "," (float (:y (nth ps 2))) "," (float (:z (nth ps 2))) ">"
         "\n"
         "texture { Red }\n"
         ;(texture g config)
         ;"\n"
         "  }"
         )))

(defn box
  [[x y] g]
  (str "box {\n"
       "\t<" x ", " y ", 0>,\n"
       "\t<" (inc x) ", " (inc y) ", " (grid/get-from g x y) ">\n"
       "\ttexture {\n"
       "\t\tT_Stone25\n"
       "\t}\n"
       "}"))

(defn mesh
  [g config]
  (str "mesh {\n"
       (s/join "\n" (map #(triangle % g config) (triangulation g)))
       "\n}"))

(defn boxes
  [g config]
  (s/join "\n" (map #(box % g) (grid-points g))))

(defn scene-description
  "Create povray scene description"
  [g config]
  (str "#include \"colors.inc\"\n"
       "#include \"stones.inc\"\n"
       "#include \"textures.inc\"\n"
       "#include \"shapes.inc\"\n"
       "#include \"glass.inc\"\n"
       "#include \"metals.inc\"\n"
       "#include \"woods.inc\"\n\n"
       (camera g config)
       "\n\n"
       (light_source g config)
       "\n\n"
       (declare-red)
       "\n\n"
       ;(mesh g config)))
       (boxes g config)))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  )

