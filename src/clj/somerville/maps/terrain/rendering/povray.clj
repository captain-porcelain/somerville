(ns somerville.maps.terrain.rendering.povray
  (:require
    [clojure.string :as s]
    [clojure.java.shell :as shell]
    [somerville.maps.terrain.rendering.conrec :as conrec]
    [somerville.geometry.point :as p]
    [somerville.geometry.polygon :as polygon]
    [somerville.maps.grid :as grid]))

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

(defn triangulation-4
  [g]
  (let [size (dec (dec (:width g)))]
    (for [x (range size)
          y (range size)
          i (range 4)]
      (let [heights (conrec/triangle-heights g x y)]
        (case i
          0 (polygon/from-points (list (:p1 heights) (:p0 heights) (:p2 heights)))
          1 (polygon/from-points (list (:p2 heights) (:p0 heights) (:p3 heights)))
          2 (polygon/from-points (list (:p3 heights) (:p0 heights) (:p4 heights)))
          3 (polygon/from-points (list (:p4 heights) (:p0 heights) (:p1 heights))))))))

(defn heights
  [g]
  (let [size (:width g)]
    (for [y (range size)
          x (range size)]
      (grid/get-from g x y))))

(defn camera
  [g config]
  (let [max-height (apply max (heights g))
        camera (p/point (/ (:width g) 2) 0 (* 1.5 max-height))
        focus  (p/point (/ (:width g) 2) (/ (:height g) 2)  0)]
    (str "camera {\n"
         "  location <" (int (:x camera)) ", " (int (:y camera)) ", " (int (:z camera)) ">\n"
         "  look_at <" (int (:x focus)) ", " (int (:y focus)) ", " (int (:z focus)) ">\n"
         "}")))

(defn light_source
  [g config]
  (str
    "light_source { <" (/ (:width g) 2) ", 0, " (* 1.5 (apply max (heights g))) "> color rgb<1, 1, 1> }"))

(defn texture
  [zs config]
  (let [m (apply max zs)]
    (cond
      (> m 100) "texture { Snow }"
      (> m 50) "texture { T_Stone10 }"
      :else "texture { T_Stone37 }")))

(defn declare-snow
  []
  (str "#declare Snow = texture {\n"
       "  pigment { White_Marble }\n"
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
         ;"texture { T_Stone37 }\n"
         (texture (map :z ps) config)
         "\n"
         "  }"
         )))

(defn mesh
  [g config]
  (str "mesh {\n"
       (s/join "\n" (map #(triangle % g config) (triangulation-4 g)))
       "\n}"))

(defn box
  [[x y] g]
  (str "box {\n"
       "\t<" x ", " y ", 0>,\n"
       "\t<" (inc x) ", " (inc y) ", " (grid/get-from g x y) ">\n"
       "\ttexture {\n"
       "\t\tT_Stone37\n"
       "\t}\n"
       "}"))

(defn grid-points
  "Get points of grid."
  [g]
  (let [size (dec (:width g))]
    (for [y (range size)
          x (range size)]
      [x y])))

(defn boxes
  "Transform the "
  [g config]
  (s/join "\n" (map #(box % g) (grid-points g))))

(defn scene-description
  "Create povray scene description"
  [g config payload]
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
       (declare-snow)
       "\n\n"
       payload))

(defn render
  [filename width height]
  (shell/sh "povray" filename (str "-W" width) (str "-H" height)))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  (let [pov (s/replace filename ".png" ".pov")]
    (do
      (spit pov (scene-description g config (mesh g config)))
      (:exit (render pov width height)))))

(defn render-blocks
  "Render the grid using blocks."
  [g config filename width height]
  (let [pov (s/replace filename ".png" ".pov")]
    (do
      (spit pov (scene-description g config (boxes g config)))
      (:exit (render pov width height)))))

