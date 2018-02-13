(ns somerville.maps.generators.dwarven-digger
  (:require
    [somerville.commons :as commons]
    [somerville.maps.grid :as grid]))


;;=======================================================================================================================
;; Find golden regions

(defn golden-neighbors
  "Find neighboring cells that contain gold."
  [g [x y]]
  (map #(vector (:x (:cell %)) (:y (:cell %))) (filter #(not (nil? (:gold (:cell %)))) (grid/neighbor-cells g (grid/get-from g [x y])))))

(defn all-golden-neighbors
  "Get set of all neighbors that contain gold for set of cells."
  [g current]
  (into #{} (reduce concat (map #(golden-neighbors g %) current))))

(defn find-golden-region
  "Find one region of gold containing cells."
  [g golden-coordinates]
  (loop [remaining golden-coordinates
         current #{(commons/get-random remaining)}
         connected #{}]
    (if (= 0 (count current))
      [connected remaining]
      (recur (remove current remaining) (clojure.set/difference (all-golden-neighbors g current) connected) (clojure.set/union connected current)))))

(defn golden-cell-coordinates
  "Find all cells containing gold."
 [g]
 (filter #(not (nil? (:gold (grid/get-from g %)))) (grid/unmasked-cell-coordinates g)))

(defn find-golden-regions
  "Find all regions containing gold."
  [g]
  (loop [remaining (golden-cell-coordinates g)
         regions '()]
    (if (= 0 (count remaining))
      regions
      (let [[region remain] (find-golden-region g remaining)]
        (recur remain (conj regions region))))))

(defn value
  "Calculate gold value of region."
  [g region]
  (reduce + (map #(:gold (grid/get-from g %)) region)))

(defn dist
  "Calculate distance between two coordinates."
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/round (Math/sqrt (+ (* dx dx) (* dy dy))))))

(defn distance
  "Calculate distance to a region."
  [region [x y]]
  (reduce min (map #(dist % [x y]) region)))


;;=======================================================================================================================
;; Placing gold in an empty grid

(defn make-gold-flooder
  "Create a flooder that places an amount of gold in a cell equal to the value minus the distance of the starting cell."
  [value]
  (fn [g c distance]
    (grid/update-cell g c #(assoc % :gold (+ (get % :gold 0) (- value distance))))))

(defn heap-gold
  "Place a heap of gold on a cell that floods over to neighboring cells."
  [g c value]
  (grid/flood g (make-gold-flooder value) [(:x c) (:y c)] value (commons/get-random (list grid/frontier-in-bounds grid/frontier-rect-in-bounds))))

(defn remove-small-regions
  "Remove gold regions consisting of only one cell."
  [g]
  (map
    #(grid/update-cell g (first (first %)) (second (first %)) (fn [c] (dissoc c :gold)))
    (filter #(= 1 (value g %)) (find-golden-regions g))))

(defn remove-trailing-ends
  "Remove gold cells with value of one that are connected to regions."
  [g]
  (map
    #(grid/update-cell g % (fn [c] (dissoc c :gold)))
    (filter
      #(and
         (< 0 (count (golden-neighbors g [(:x %) (:y %)])))
         (= 1 (get % :gold 0)))
      (grid/all-cells g))))

(defn place-gold
  "Place heaps of gold in grid."
  [g config]
  (let [cells (grid/unmasked-cells g)
        random-cells (repeatedly (:amount config) #(commons/get-random cells))
        t (dorun (map #(heap-gold g % (commons/get-random (:gold-distribution config))) random-cells))
        t (when (:remove-small-regions config) (dorun (remove-small-regions g)))
        t (when (:remove-trailing-ends config) (dorun (remove-trailing-ends g)))]
    g))

;;=======================================================================================================================
;; Post processing
;;

(defn open-cells
  "Find all open cells."
  [g]
  (filter #(< 0 (count (get % :links []))) (grid/all-cells g)))

(defn unconnected-open-neighbors
  "Find open neighbors that are not linked to cell."
  [g c]
  (let [open (filter #(< 0 (count (get (:cell %) :links []))) (grid/neighbor-cells g c))]
    (filter #(not (commons/in? (:direction %) (:links c))) open)))

(defn open-to-neighbors
  "Add link to neighbors."
  [g c neighbors]
  (dorun (map #(grid/place-link-to g c (:cell %)) neighbors)))

(defn remove-thin-walls
  "Remove walls that are inside one room."
  [g]
  (dorun
    (map #(open-to-neighbors g % (unconnected-open-neighbors g %)) (open-cells g))))

;;=======================================================================================================================
;; Create a dungeon
;;
;; Assume to be a dwarf. You have a good sense in which direction to find gold and how far away it is.
;; You dig towards the closest and biggest region. But since you are drunk there is always the chance that you
;; take a wrong turn. You also have some issues diciding which way to go when two regions are the same distance.
;; So you call another dwarf to help you dig both ways.

;(defn non-border-neighbors
  ;"Get neighbor coordinates that are not on the border."
  ;[g c]
  ;(let [border (grid/unmasked-border-coordinates g)
        ;candidates (filter #(not (commons/in? [(:x (:cell %)) (:y (:cell %))] border)) (grid/neighbor-cells g (grid/get-from g c)))]
    ;candidates))

(defn tap-cell
  "Create links from a cell to all neighbors containing gold."
  [g [x y]]
  (dorun (map #(grid/place-link-to g (grid/get-from g [x y]) (grid/get-from g %)) (golden-neighbors g [x y]))))

(defn tap-region
  "Ensure that a whole region is connected."
  [g region]
  (map #(tap-cell g %) region))

(defn best-candidate
  "Find the candidate with the shortest distance to another region. If you are drunk you only have a 50% chance"
  [g candidates untapped]
  (if (= 0 (count untapped))
    (commons/get-random candidates)
    (let [sorted-candidates (sort-by :shortest
                                     (map #(hash-map :candidate %
                                                     :shortest (reduce min (map (fn [r] (distance r %)) untapped)))
                                          candidates))
          probability (if (:drunk (:config g)) 2 1)]
      (:candidate
        (if (= 0 (rand-int probability))
          (first sorted-candidates)
          (commons/get-random sorted-candidates))))))

(defn carve
  "Carve tunnels that connect golden regions."
  [g start]
  (loop [untapped (find-golden-regions g)
         dungeon #{}
         c start]
    (if (= 0 (count untapped))
      g
      (let [candidates (grid/neighbor-cells g (grid/get-from g c))]
        (if (= 0 (count candidates))
          g
          (let [c2 (best-candidate g (map #(vector (:x (:cell %)) (:y (:cell %))) candidates) untapped)
                tmp (grid/place-link-to g (grid/get-from g c) (grid/get-from g c2))
                region (first (filter #(commons/in? c2 %) untapped))
                tmp (when-not (nil? region) (dorun (tap-region g region)))
                remaining (if (not (nil? region)) (remove #{region} untapped) untapped)
                dungeon2 (if (not (nil? region)) (clojure.set/union dungeon region) dungeon)
                next-cell (if (nil? region) c2 (best-candidate g (into (list) dungeon) remaining))]
            (recur remaining (clojure.set/union dungeon2 #{c2}) next-cell)))))))

(defn random-start
  "Get a random starting position from the border of the grid that does not contain gold."
  [g]
  (grid/get-from g (commons/get-random (filter #(nil? (:gold (grid/get-from g %))) (grid/unmasked-border-coordinates g)))))

;;=======================================================================================================================
;; Interface of the Dwarven Digger

(defn default-config
  "Generate default configuration for a dungeon."
  [width height]
  {:gold-distribution [1 1 3 4]
   :remove-small-regions false
   :remove-trailing-ends true
   :remove-thin-walls true
   :drunk true
   :width width
   :height height
   :grid-type :hex
   :iterations 1
   :amount (int (* 0.025 width height))})

(defn dig
  "Place gold in grid and let loose the drawves of digging."
  [g]
   (let [t (place-gold g (:config g))
         start (:start g)
         tmp (dorun (carve g [(:x start) (:y start)]))]
     g))

(defn process
  "Handle the process of digging the dungeon."
  [config]
   (let [g (case (:grid-type config)
             :rect (grid/grid (:width config) (:height config))
             :hex  (grid/hex-grid (:width config) (:height config)))
         start (random-start g)
         g2 (assoc g :start start :config config)
         tmp (grid/make-entrance g2 start)
         tmp (dorun (map (fn [i] (dig g2)) (range (:iterations config))))
         tmp (when (:remove-thin-walls config) (remove-thin-walls g))]
     g2))

(defn dungeon
  "Create a dungeon based on a dwarven digger searching for gold."
  ([config]
   (process config))
  ([width height]
   (process (default-config width height)))
  ([width height grid-type]
   (process (assoc (default-config width height) :grid-type grid-type))))
