;; Provides the facilities to manipulate images
(ns somerville.maps.rendering.tiles
  (:import
    [java.awt Graphics2D]
    [java.awt.image BufferedImage])
  (:require
    [somerville.commons :as commons]
    [somerville.image :as image]
    [somerville.maps.grid :as grid]))

(defn image-size-rect
  "Calculate size of resulting image."
  [tile-names tileset]
  (let [tile-size (image/size-resource (str "tiles/rect/" tileset "/00000000.png"))]
    [(* (count (first tile-names)) (:width tile-size))
    (* (count tile-names) (:height tile-size))
    (:width tile-size)
    (:height tile-size)]))

(defn tile-size-hex
  "Get size of one hex tile."
  [tileset]
  (image/size-resource (str "tiles/hex/" tileset "/000000.png")))

(defn tilesets
  "List available tilesets."
  [grid-type]
  (commons/list-resources (str "tiles/" grid-type)))

(defn tile-type
  [filename]
  (first (clojure.string/split (first (clojure.string/split filename #"\.")) #"-")))

(defn load-tile-types
  "Load all tiles in a tileset."
  [tileset grid-type]
  (map #(vector (tile-type %) (str "tiles/" grid-type "/" tileset "/" %)) (sort (commons/list-resources (str "tiles/" grid-type "/" tileset)))))

(defn load-tiles-map
  "Load all tiles in a tileset."
  [tileset grid-type]
  (into {} (map #(vector (first (first %)) (map second %)) (partition-by first (load-tile-types tileset grid-type)))))

(defn load-tiles
  "Load all tiles in a tileset."
  [tileset grid-type]
  (into {} (map (fn [[k v]] [k (map #(image/load-image-resource %) v)]) (load-tiles-map tileset grid-type))))

(defn new-image
  "Create a new image to hold the finished tiles."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))

(defn place-tiles-rect
  [tiles tile-names ^Graphics2D graphics ^Integer width ^Integer height]
  (for [y (range (count tile-names))
        x (range (count (first tile-names)))]
    (let [w (int (* x width))
          h (int (* y height))
          tn (nth (nth tile-names y) x)
          t (rand-nth (get tiles tn))]
      (if (nil? t)
        (dorun (println (str "Missing tile " tn)))
        (.drawImage graphics t w h nil)))))

(defn render-tiles-rect
  "Render rect tiles into one image."
  [tile-names tileset imagename]
  (let [tiles (load-tiles tileset "rect")
        [width height tw th] (image-size-rect tile-names tileset)
        img ^BufferedImage (new-image width height)
        graphics ^Graphics2D (.createGraphics img)
        tmp (dorun (place-tiles-rect tiles tile-names graphics tw th))
        tmp (.dispose graphics)]
    (image/write-image imagename img)))

(defn place-tiles-hex
  [tiles tile-names ^Graphics2D graphics ^Integer width ^Integer height]
  (for [y (range (count tile-names))
        x (range (count (first tile-names)))]
    (let [w (int (* x 3 (/ width 4)))
          h (int (* y height))
          h (if (even? x) (int (+ h (/ height 2))) h)
          tn (nth (nth tile-names y) x)
          t (rand-nth (get tiles tn))]
      (if (nil? t)
        (dorun (println (str "Missing tile " tn)))
        (.drawImage graphics t w h nil)))))

(defn render-tiles-hex
  "Render hex tiles into one image."
  [tile-names tileset imagename]
  (let [tiles (load-tiles tileset "hex")
        tilesize (tile-size-hex tileset)
        iw (* (:width tilesize) (count (first tile-names)))
        ih (* (:height tilesize) (inc (count tile-names)))
        img ^BufferedImage (new-image iw ih)
        graphics ^Graphics2D (.createGraphics img)
        tmp (dorun (place-tiles-hex tiles tile-names graphics (:width tilesize) (:height tilesize)))
        tmp (.dispose graphics)]
    (image/write-image imagename img)))

(defn tile-name-rect
  "Create the name of the tile file to draw this cell."
  [g c]
  (let [ce (grid/linked-cell g c :east)
        cn (grid/linked-cell g c :north)
        cw (grid/linked-cell g c :west)
        cs (grid/linked-cell g c :south)]
    (str
      (if (or (not (commons/in? :south (:links c))) (not (commons/in? :west  (:links c))) (not (commons/in? :west (:links cs))) (not (commons/in? :south (:links cw)))) "1" "0")
      (if (commons/in? :west  (:links c)) "0" "1")
      (if (or (not (commons/in? :west  (:links c))) (not (commons/in? :north (:links c))) (not (commons/in? :west (:links cn))) (not (commons/in? :north (:links cw)))) "1" "0")
      (if (commons/in? :north (:links c)) "0" "1")
      (if (or (not (commons/in? :north (:links c))) (not (commons/in? :east  (:links c))) (not (commons/in? :east (:links cn))) (not (commons/in? :north (:links ce)))) "1" "0")
      (if (commons/in? :east  (:links c)) "0" "1")
      (if (or (not (commons/in? :east  (:links c))) (not (commons/in? :south (:links c))) (not (commons/in? :east (:links cs))) (not (commons/in? :south (:links ce)))) "1" "0")
      (if (commons/in? :south (:links c)) "0" "1"))))

(defn tile-name-hex
  "Create the name of the tile file to draw this cell."
  [g c]
  (str
    (if (commons/in? :south-west  (:links c)) "0" "1")
    (if (commons/in? :south       (:links c)) "0" "1")
    (if (commons/in? :south-east  (:links c)) "0" "1")
    (if (commons/in? :north-east  (:links c)) "0" "1")
    (if (commons/in? :north       (:links c)) "0" "1")
    (if (commons/in? :north-west  (:links c)) "0" "1")))

(defn tile-name-walker
  "Create the tile names for each cell."
  [g c]
  (grid/set-in g (:x c) (:y c) (assoc c :tile (if (= :rect (:grid-type g)) (tile-name-rect g c) (tile-name-hex g c)))))

(defn tiles
  "Create representation of grid with tiles."
  [g]
  (let [g2 (grid/walk g tile-name-walker)]
    (for [y (range (:height g2))]
      (for [x (range (:width g2))]
        (:tile (grid/get-from g2 x y))))))

(defn render-grid
  "Render grid with tiles to image."
  [g tileset imagename]
  (if (= :rect (:grid-type g))
    (render-tiles-rect (tiles g) tileset imagename)
    (render-tiles-hex  (tiles g) tileset imagename)))
