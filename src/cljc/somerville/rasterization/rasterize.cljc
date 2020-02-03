;; Provides functions for handling geometric objects.
(ns somerville.rasterization.rasterize)

(defn circlefn
  "Build a function that calculates the positive y values at x for a circle with a radius of r."
  [^Integer r]
  (fn [x] (Math/round (Math/sqrt (- (* r r) (* x x))))))

(defn linefn
  "Build a function that calculates the y values at x for a line from [x1, y1] to [x2, y2]."
  [[^Integer x1 ^Integer y1] [^Integer x2 ^Integer y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        a (/ dy dx)
        b (- y1 (* a x1))]
    (fn [x] (Math/round ^Double (+ (* a x) b)))))

(defn cnext
  "Given a function and a pixel on the graph of that function get the next pixel with potentially growing x."
  [f ^Integer x ^Integer y]
  (let [xt (+ x 1)
        y2 (f xt)
        dy (- y2 y)
        y2 (if (> dy 1) (+ y 1) y2)
        y2 (if (< dy -1) (- y 1) y2)
        x2 (if (= y y2) xt x)]
    [x2 y2]))

(defn rasterize
  "A lazy sequence of pixels approximating a given function between two points."
  [[^Integer x1 ^Integer y1] [^Integer x2 ^Integer y2] f]
  (if (= [x1 y1] [x2 y2])
    (list [x1 y1])
    (lazy-seq (concat (list [x1 y1]) (rasterize (cnext f x1 y1) [x2 y2] f)))))

(defn circle-q2
  "Get the pixels constituting a circle in quadrant 2 with a given radius centered in [0, 0]."
  [^Integer r]
  (let [start [(* -1 r) 0]
        end [0 r]
        f (circlefn r)]
    (rasterize start end f)))

(defn line-q1
  "Get the pixels of a line from [0, 0] t o [x y]."
  [^Integer x ^Integer y]
  (if (= 0 x)
    (map (fn [v] [0 v]) (take (+ 1 y) (iterate inc 0)))
    (rasterize [0 0] [x y] (linefn [0 0] [x y]))))

(defn flip-y
  "Flip the y component of a pixel."
  [[^Integer x ^Integer y]]
  [x (* -1 y)])

(defn flip-x
  "Flip the x component of a pixel."
  [[^Integer x ^Integer y]]
  [(* -1 x) y])

(defn translate
  "Move a point."
  [[^Integer px ^Integer py] [^Integer mx ^Integer my]]
  [(+ px mx) (+ py my)])

(defn translate-line
  "Move the points of a line."
  [l [^Integer mx ^Integer my]]
  (map #(translate % [mx my]) l))

(defn translate-lines
  "Move the points of a list of lines."
  [l [^Integer mx ^Integer my]]
  (map #(translate-line % [mx my]) l))

(defn circle
  "Get the pixels constituting a full circle with a given radius centered in [0, 0]."
  [^Integer r]
  (let [quarter (circle-q2 r)
        half1 (concat quarter (reverse (map #(flip-x %) quarter)))
        half2 (butlast (rest (reverse (map #(flip-y %) half1))))]
    (concat half1 half2)))

(defn line
  "Get the pixels approximating a line from start to end."
  [[^Integer x1 ^Integer y1] [^Integer x2 ^Integer y2]]
  (let [xt (- x2 x1)
        yt (- y2 y1)
        flipfn (if (< xt 0) flip-x identity)
        flipfn (if (< yt 0) (fn [x] (flip-y (flipfn x))) flipfn)
        [x y] (flipfn [xt yt])]
    (map (fn [[x y]] [(+ x1 x) (+ y1 y)]) (map flipfn (line-q1 x y)))))

(defn sight-lines-internal
  "Create the array of sight lines for a circle given by radius r."
  [^Integer r]
  (map #(line [0 0] %) (circle r)))

(def sight-lines (memoize sight-lines-internal))

;; testing functions for creating a tree instead of naive list of lines.

(defn insert-nodes
  "Create a lazy tree from the list of sightlines."
  [sl]
  (loop [lines sl
         nodes (list)
         counter 0]
    (if (or (nil? lines) (= 0 (count lines)))
      nodes
      (let [lines (sort-by #(first %) lines)
            f (first (first lines))
            children (map rest (filter #(= f (first %)) lines))
            rests (filter #(not (= f (first %))) lines)
            children (if (< 0 (count (first children))) (insert-nodes children) nil)
            node {:p f :c children}
            ]
        (recur rests (cons node nodes) (+ counter 1))))))

(defn sight-lines-tree
  "Reduce the duplicates by transforming the list of lists into a tree."
  [^Integer r]
  (first (insert-nodes (sight-lines r))))

(defn count-pixels
  "Count the pixels in a list of lists of pixels."
  [viewlines]
  (reduce + (map #(count %) viewlines)))

(defn count-pixels-tree
  "Count the pixels in a tree."
  [t]
  (+ 1 (if (not (nil? (:c t))) (reduce + (map #(count-pixels-tree %) (:c t))) 0)))

(defn in-tree?
  "Check if a pixel is contained in the tree."
  [t p]
  (let [isp (= p (:p t))
        inch (if (not isp) (true? (some true? (map #(in-tree? % p) (:c t)))) isp)]
    inch))

(defn line-full?
  "Check if all pixels of a line are contained in the tree."
  [t l]
  (every? true? (map #(in-tree? t %) l)))

(defn tree-full?
  "Check if all pixels in a list of lists of pixels are contained in the tree."
  [t sl]
  (every? true? (map #(line-full? t %) sl)))

