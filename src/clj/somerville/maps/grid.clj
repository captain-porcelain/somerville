;; Provides two dimensional grids and functions to manipulate them
(ns somerville.maps.grid
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [taoensso.timbre :as log]))

;==================================================================================================================
; Grid Cell Access

(defn in-bounds?
  "Check if cell is in grid and not masked."
  ([g x y]
   (and
     (< y (:height g))
     (< x (:width g))
     (<= 0 x)
     (<= 0 y))
     (not (:masked (try (aget (:array g) x y) (catch Exception e {:masked true})))))
  ([g c]
   (in-bounds? g (p/x c) (p/y c))))

(defn get-from
  "Get an element from grid array."
  ([g w h]
   (if (and (not (nil? w)) (not (nil? h)) (< -1 w) (< w (:width g)) (< -1 h) (< h (:height g)))
     (aget (:array g) w h)
     {}))
  ([g [x y]]
   (get-from g x y)))

(defn set-in
  "Update an element in grid array."
  [g w h e]
  (aset (:array g) w h e))

(defn set-integer
  "Update an element in grid array."
  [g w h e]
  (aset (:array g) w h (int e)))

(defn update-cell
  "Update a cell."
  ([g w h f]
   (set-in g w h (f (get-from g w h))))
  ([g c f]
   (set-in g (p/x c) (p/y c) (f (get-from g (p/x c) (p/y c))))))


;==================================================================================================================
; Grid Cell Filters

(defn all-cells
  "Get all cells from grid."
  [g]
  (for [y (range (:height g))
        x (range (:width g))]
    (get-from g x y)))

(defn dead-ends
  "Get the dead ends in the grid."
  [g]
  (filter #(= 1 (count (:links %))) (all-cells g)))

(defn blocked-cells
  "Get the cells without neighbors in the grid."
  [g]
  (filter #(= 0 (count (:links %))) (all-cells g)))

(defn unmasked-cell-coordinates
  "Get all cell coordinates that are not masked."
  [g]
  (map #(vector (p/x %) (p/y %)) (filter #(not (:masked %)) (all-cells g))))

(defn masked-cell-coordinates
  "Get all cell coordinates that are not masked."
  [g]
  (map #(vector (p/x %) (p/y %)) (filter #(:masked %) (all-cells g))))

(defn unmasked-cells
  "Get all cell coordinates that are not masked."
  [g]
  (filter #(not (:masked %)) (all-cells g)))

(defn unmasked-border-coordinates
  "Get all cell coordinates that are on the grid border and not masked"
  [g]
  (filter #(or (= 0 (first %)) (= 0 (second %)) (= (dec (:width g)) (first %)) (= (dec (:height g)) (second %)))
    (unmasked-cell-coordinates g)))


;==================================================================================================================
; Grid based on two dimensional array

(defn grid-map
  "Create a two dimensional grid of rectangles sized width x height."
  [^Integer width ^Integer height grid-type]
  {:width width
   :height height
   :grid-type grid-type
   :data-type :map
   :array (to-array-2d (repeat width (repeat height {})))})

(defn integer-grid-map
  "Create a two dimensional grid of integers."
  [^Integer width ^Integer height grid-type]
  {:width width
   :height height
   :grid-type grid-type
   :data-type :int
   :array (make-array Integer/TYPE width height)})

(defn int-grid
  "Create a two dimensional grid with cells of rectangles sized width x height."
  [^Integer width ^Integer height grid-type]
  (let [g (grid-map width height grid-type)
        tmp (dorun
              (for [y (range (:height g))
                    x (range (:width g))]
                (set-in g x y {:links [] p/x x p/y y})))]
    g))

(defn grid
  "Create a two dimensional grid with cells of rectangles sized width x height."
  [^Integer width ^Integer height]
  (int-grid width height :rect))

(defn hex-grid
  "Create a two dimensional grid with cells of rectangles sized width x height."
  [^Integer width ^Integer height]
  (int-grid width height :hex))

(defn integer-grid
  "Create a two dimensional grid of Integers of width x height."
  [^Integer width ^Integer height]
  (integer-grid-map width height :rect))

;==================================================================================================================
; Maze Building Helpers

(defn linked-cell
  "Get the coordinates for the linked cell."
  [g c link]
  (get-from g
    (case link
      :east       [(inc (p/x c)) (p/y c)]
      :west       [(dec (p/x c)) (p/y c)]
      :south      [(p/x c)       (inc (p/y c))]
      :north      [(p/x c)       (dec (p/y c))]
      :north-west (if (odd? (p/x c)) [(dec (p/x c)) (dec (p/y c))] [(dec (p/x c)) (p/y c)])
      :south-west (if (odd? (p/x c)) [(dec (p/x c)) (p/y c)]       [(dec (p/x c)) (inc (p/y c))])
      :north-east (if (odd? (p/x c)) [(inc (p/x c)) (dec (p/y c))] [(inc (p/x c)) (p/y c)])
      :south-east (if (odd? (p/x c)) [(inc (p/x c)) (p/y c)]       [(inc (p/x c)) (inc (p/y c))]))))

(defn link-from-coordinates
  "Get the link from the coordinates of two cells."
  [[x1 y1] [x2 y2] grid-type]
  (case grid-type
    :rect (cond
            (and (= x1 x2) (= y1 (inc y2))) :north
            (and (= x1 x2) (= y1 (dec y2))) :south
            (and (= x1 (inc x2)) (= y1 y2)) :west
            (and (= x1 (dec x2)) (= y1 y2)) :east)
    :hex  (cond
            (and            (= x1 x2)       (= y1 (inc y2))) :north
            (and            (= x1 x2)       (= y1 (dec y2))) :south
            (and (odd? x1)  (= x1 (inc x2)) (= y1 (inc y2))) :north-west
            (and (odd? x1)  (= x1 (inc x2)) (= y1 y2))       :south-west
            (and (odd? x1)  (= x1 (dec x2)) (= y1 (inc y2))) :north-east
            (and (odd? x1)  (= x1 (dec x2)) (= y1 y2))       :south-east
            (and (even? x1) (= x1 (inc x2)) (= y1 y2))       :north-west
            (and (even? x1) (= x1 (inc x2)) (= y1 (dec y2))) :south-west
            (and (even? x1) (= x1 (dec x2)) (= y1 y2))       :north-east
            (and (even? x1) (= x1 (dec x2)) (= y1 (dec y2))) :south-east)))

(defn neighbor-cells
  "Get accessible neighbors of a cell."
  [g c]
  (if (= {} c)
    (list)
    (filter #(and (not (= {} (:cell %))) (in-bounds? g (:cell %)))
    (case (:grid-type g)
      :rect (list
              {:direction :east  :cell (linked-cell g c :east)}
              {:direction :west  :cell (linked-cell g c :west)}
              {:direction :north :cell (linked-cell g c :north)}
              {:direction :south :cell (linked-cell g c :south)})
      :hex  (list
              {:direction :north-east  :cell (linked-cell g c :north-east)}
              {:direction :south-east  :cell (linked-cell g c :south-east)}
              {:direction :north-west  :cell (linked-cell g c :north-west)}
              {:direction :south-west  :cell (linked-cell g c :south-west)}
              {:direction :north       :cell (linked-cell g c :north)}
              {:direction :south       :cell (linked-cell g c :south)})))))

(defn unvisited-neighbors
  "Get neighbors that have not been visited before."
  [g c]
  (filter #(and (= 0 (count (:links (:cell %)))) (not (:masked (:cell %))))
          (neighbor-cells g c)))

(defn visited-neighbors
  "Get neighbors that have been visited before."
  [g c]
  (filter #(and (< 0 (count (:links (:cell %)))) (not (:masked (:cell %))))
          (neighbor-cells g c)))

(defn surrounding-coordinates
  "Get the coordinates of surrounding cells."
  [g x y]
  (filter
    #(in-bounds? g (first %) (second %))
    (case (:grid-type g)
      :rect (list [(dec x) (dec y)]
                  [x (dec y)]
                  [(inc x) (inc y)]
                  [(dec x) y]
                  [(inc x) y]
                  [(dec x) (inc y)]
                  [x (inc y)]
                  [(inc x) (inc y)])
      :hex (map #(vector (p/x (:cell %)) (p/y (:cell %))) (neighbor-cells g (get-from g x y))))))

(defn reverse-link
  "Get the reverse of a link direction."
  [link]
  (case link
    :east       :west
    :west       :east
    :south      :north
    :north      :south
    :north-west :south-east
    :south-west :north-east
    :north-east :south-west
    :south-east :north-west
    nil         nil))

(defn place-link-to
  "Connect grid cells."
  [g c c2]
  (let [link (link-from-coordinates  [(p/x c) (p/y c)] [(p/x c2) (p/y c2)] (:grid-type g))]
    [(update-cell g c  #(assoc % :links (conj (:links %) link)))
     (update-cell g c2 #(assoc % :links (conj (:links %) (reverse-link link))))]))

(defn place-link
  "Connect grid cell to the one indicated by the link."
  [g c link]
  (when-not (nil? link) (place-link-to g c (linked-cell g c link))))

(defn unlink
  "Unconnect grid cell from the on indicated by the link."
  [g c link]
  (when-not (nil? link)
    (let [c2 (linked-cell g c link)
          l1 (update-cell g c #(assoc % :links (remove #{link} (:links %))))
          l2 (update-cell g c2 #(assoc % :links (remove #{(reverse-link link)} (:links %))))]
      )))

(defn unlink-all
  "Unconnect grid cell from all connected neighbors."
  [g c]
  (map #(unlink g c %) (:links c)))

(defn masked?
  "Check if a cells should be masked."
  [mask c]
  (= \X (nth (nth mask (p/y c) " ") (p/x c) " ")))

(defn make-mask-walker
  "Create a walker that marks cells as masked based on a text.
  Each line of text represents one grid row. A X marks the cell as masked."
  [text]
  (let [mask (clojure.string/split text #"\n")]
    (fn [g e]
      (update-cell g e #(assoc % :masked (masked? mask e))))))

(defn make-entrance
  "Ensure that the start cell connects to outside of the grid."
  [g c]
  (case (:grid-type g)
    :rect (cond
            (= (p/y c) 0)                 (update-cell g c #(assoc % :links (conj (:links %) :north)))
            (= (p/y c) (dec (:height g))) (update-cell g c #(assoc % :links (conj (:links %) :south)))
            (= (p/x c) 0)                 (update-cell g c #(assoc % :links (conj (:links %) :west)))
            (= (p/x c) (dec (:width g)))  (update-cell g c #(assoc % :links (conj (:links %) :east))))
    :hex (cond
            (= (p/y c) 0)                 (update-cell g c #(assoc % :links (conj (:links %) :north)))
            (= (p/y c) (dec (:height g))) (update-cell g c #(assoc % :links (conj (:links %) :south)))
            (= (p/x c) 0)                 (update-cell g c #(assoc % :links (concat (:links %) (list :south-west :north-west))))
            (= (p/x c) (dec (:width g)))  (update-cell g c #(assoc % :links (concat (:links %) (list :south-east :north-east)))))))

(defn sparse
  "Make a grid sparse by eliminating dead ends until the factor of rock cells to path cells reaches the given factor."
  [g factor]
  (loop [des (dead-ends g)]
    (if (or (> (/ (count (blocked-cells g)) (* (:width g) (:height g))) factor) (= 0 (count des)))
      g
      (let [tmp (dorun (map #(dorun (unlink-all g %)) des))]
        (recur (dead-ends g))))))


;==================================================================================================================
; Grid Iterators

(defn walk
  "Execute function for every element in grid and return grid.
  The function should take the grid and the current element as arguments."
  [g f]
  (dorun
    (for [y (range (:height g))
          x (range (:width g))]
      (f g (get-from g x y))))
  g)

(defn walk-result
  "Execute function for every element in grid and return walking result.
  The function should take the grid and the current element as arguments."
  [g f]
  (for [y (range (:height g))
        x (range (:width g))]
    (f g (get-from g x y))))

(defn walk-lines
  "Execute function for every line in grid.
  The function should take the grid and the current row of elements as arguments."
  [g f]
  (dorun
    (for [y (range (:height g))]
      (f g (map #(get-from g % y) (range (:width g))))))
  g)

(defn iterate-all
  "Execute function for every unmasked cell in grid.
  The function should take the grid, current cell and the remaining cells as arguments and return the next cell."
  [g start f]
  (loop [remaining (unmasked-cell-coordinates g)
         [x y] start]
    (if (= 0 (count remaining))
      g
      (let [next-cell (f g (get-from g x y) remaining)]
        (recur (remove #{next-cell} remaining) next-cell)))))

(defn frontier-in-bounds
  "Build a set of new frontier cells for grid based from position w, h ignoring walls."
  [g [x y] visited]
  (into #{}
    (filter #(not (commons/in? % visited))
            (map #(vector (p/x (:cell %)) (p/y (:cell %)))
                 (neighbor-cells g (get-from g x y))))))

(defn frontier-rect-in-bounds
  "Build a set of new frontier cells for grid based from position w, h ignoring walls."
  [g [x y] visited]
  (into #{}
    (filter #(not (commons/in? % visited))
      (surrounding-coordinates g x y))))

(defn frontier-no-wall
  "Build a set of new frontier cells for grid based from position w, h and ignore already visited cells."
  [g [x y] visited]
  (into #{}
    (filter #(not (commons/in? % visited))
            (map #(vector (p/x (:cell %)) (p/y (:cell %)))
                 (filter
                   #(commons/in? (:direction %) (:links (get-from g x y)))
                   (neighbor-cells g (get-from g x y)))))))

(defn flood
  "Execute funtion for every element in the grid by building a frontier set from a starting node outwards.
  The function should take the grid, the current element and the distance to the first cell as arguments."
  ([g f start max-distance frontier-fn]
    (loop [frontier #{start}
           visited #{}
           distance 0]
      (if (or (= 0 (count frontier)) (= distance max-distance))
        g
        (let [new-frontier (reduce concat (map #(frontier-fn g % visited) frontier))
              new-visited (clojure.set/union visited frontier)
              new-distance (inc distance)
              tmp (dorun (map #(f g (get-from g (first %) (second %)) distance) frontier))]
          (recur new-frontier new-visited new-distance)))))
  ([g f start max-distance]
   (flood g f start max-distance frontier-no-wall))
  ([g f start]
   (flood g f start -1 frontier-no-wall))
  ([g f]
   (flood g f (first (unmasked-cell-coordinates g)) -1 frontier-no-wall)))


