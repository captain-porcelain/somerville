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
      (:z (grid/get-from g x y)))))

(defn camera
  [g width height config]
  (let [max-height (apply max (heights g))
        detail (:detail (:config g))
        camera (p/point (/ (:width g) 2) (* 4 (/ (:height g) 5)) (* (/ detail 1.8) max-height))
        focus  (p/point (/ (:width g) 2) (/ (:height g) 2) 0)]
    (str "camera {\n"
         "  location <" (* -1 (float (:x camera))) ", " (* -1 (float (:y camera))) ", " (float (:z camera)) ">\n"
         "  look_at <" (* -1 (float (:x focus))) ", " (* -1 (float (:y focus))) ", " (float (:z focus)) ">\n"
         "  right image_width/image_height*x\n"
         "}")))

(defn light_source
  [g config]
  (str
    "light_source { <" (* -1 (/ (:width g) 2)) ", 0, " (* 1.5 (apply max (heights g))) "> color rgb<1, 1, 1> }"))

(defn background
  [config]
  (str
    "background { rgbft <"
    (float (/ (nth (:background-color config) 0) 255)) ","
    (float (/ (nth (:background-color config) 1) 255)) ","
    (float (/ (nth (:background-color config) 2) 255)) ","
    (float (/ (nth (:background-color config) 3) 255))
    "> }"))

(defn texture
  "Select appropriate texture based on height."
  [m config]
  (str "texture { " (second (first (filter #(< (first %) m) (:level-map config)))) " }"))

(defn declare-textures
  []
  (str
    "#declare HT_Water = texture {\n"
    "  Water\n"
    "  finish { ambient 0.2 diffuse 0.1 }\n"
    "}"
    "#declare HT_Snow = texture {\n"
    "  pigment { White_Marble }\n"
    "  finish { ambient 0.2 diffuse 0.1 }\n"
    "}"
    "#declare HT_Green = texture {\n"
    "  pigment { Jade }\n"
    "  finish { ambient 0.2 diffuse 0.1 }\n"
    "}"
    "#declare HT_Rock = texture {\n"
    "  T_Stone10\n"
    "  finish { ambient 0.2 diffuse 0.1 }\n"
    "}"
    "#declare HT_Sand = texture {\n"
    "  T_Stone37\n"
    "  finish { ambient 0.2 diffuse 0.1 }\n"
    "}"
    "#declare HT_DarkStone = texture {\n"
    "  T_Stone31\n"
    "  finish { ambient 0.2 diffuse 0.1 }\n"
    "}"
    ))

(defn level-z
  "Ensure z value is not below low-height."
  [z config]
  (if (< z (:water-height config)) (:water-height config) z))

(defn triangle
  [polygon g config]
  (let [ps (polygon/to-points polygon)]
    (str "  triangle { "
         "<" (* -1 (float (:x (nth ps 0)))) "," (* -1 (float (:y (nth ps 0)))) "," (float (level-z (:z (nth ps 0)) config)) ">, "
         "<" (* -1 (float (:x (nth ps 1)))) "," (* -1 (float (:y (nth ps 1)))) "," (float (level-z (:z (nth ps 1)) config)) ">, "
         "<" (* -1 (float (:x (nth ps 2)))) "," (* -1 (float (:y (nth ps 2)))) "," (float (level-z (:z (nth ps 2)) config)) "> "
         (texture (apply max (map :z ps)) config)
         " }")))

(defn mesh
  [g config]
  (str "mesh {\n"
       (s/join "\n" (map #(triangle % g config) (triangulation g)))
       "\n}"))

(defn box
  [[x y] g config]
  (str "box {"
       "<" x ", " y ", 0>, "
       "<" (inc x) ", " (inc y) ", " (float (level-z (:z (grid/get-from g x y)) config)) "> "
       (texture (float (:z (grid/get-from g x y))) config)
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
  (s/join "\n" (map #(box % g config) (grid-points g))))

(defn scene-description
  "Create povray scene description"
  [g config width height payload]
  (str "#include \"colors.inc\"\n"
       "#include \"stones.inc\"\n"
       "#include \"textures.inc\"\n"
       "#include \"shapes.inc\"\n"
       "#include \"glass.inc\"\n"
       "#include \"metals.inc\"\n"
       "#include \"woods.inc\"\n\n"
       (camera g width height config)
       "\n\n"
       (light_source g config)
       "\n\n"
       (background config)
       "\n\n"
       (declare-textures)
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
      (spit pov (scene-description g config width height (mesh g config)))
      (:exit (render pov width height)))))

(def default-config
  {:background-color      [0 0 0 255]
   :water-height          10
   :level-map             [[32 "HT_DarkStone"]
                           [28 "HT_Rock"]
                           [20 "HT_Green"]
                           [0 "HT_Sand"]
                           [-1000 "HT_Water"]]})

(defn render-blocks
  "Render the grid using blocks."
  [g config filename width height]
  (let [pov (s/replace filename ".png" ".pov")]
    (do
      (spit pov (scene-description g config width height (boxes g config)))
      (:exit (render pov width height)))))

