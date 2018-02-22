(ns somerville.data-structures.array)

;; For these macros see http://www.bestinclass.dk/index.clj/2010/03/functional-fluid-dynamics-in-clojure.html
;; Basically these macros provide the functionality to get and set double values in a two dimensional array
;; without reflection for the values. This is done for performance of course.
(defmacro aget!
  ([array x]      `(aget ~(vary-meta array assoc :tag 'doubles) ~x))
  ([array x y] `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~x)]
                     (aget! a# ~y))))

(defmacro aset!
  ([array x v] `(aset ~(vary-meta array assoc :tag 'doubles) ~x (double ~v)))
  ([array x y v] (let [nested-array `(aget ~(vary-meta array assoc :tag 'objects) ~x)
                       a-sym         (with-meta (gensym "a") {:tag 'doubles})]
                   `(let [~a-sym ~nested-array]
                      (aset ~a-sym ~y (double ~v))))))

(defn- split-height
  [array slice x h]
  (if (= 0 (count slice))
    array
    (let [y (- h (count slice))
          tmp (aset! array x y (first slice))]
      (recur array (rest slice) x h))))

(defn- reshape
  [array slices w h]
  (if (= 0 (count slices))
    array
    (recur (split-height array (first slices) (- w (count slices)) h) (rest slices) w h)))

(defn reshape-to-2d-array
  "Takes a list of doubles and reshapes them into a 2d array of width and height"
  [coll width height]
  (let [array (make-array Double/TYPE width height)
        slices (partition height coll)]
    (reshape array slices width height)))
