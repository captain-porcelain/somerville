(ns sanakan.algebra.array)

;; For these macros see http://www.bestinclass.dk/index.clj/2010/03/functional-fluid-dynamics-in-clojure.html
;; Basically these macros provide the functionality to get and set double values in a two dimensional array
;; without reflection for the values. This is done for performance of course.
(defmacro aget!
  ([array x]      `(aget ~(vary-meta array assoc :tag 'doubles) ~x))
  ([array x y] `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~x)]
                     (aget! a# ~y))))

(defmacro aset! [array x y v]
  (let [nested-array `(aget ~(vary-meta array assoc :tag 'objects) ~x)
        a-sym         (with-meta (gensym "a") {:tag 'doubles})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~y (double ~v)))))

