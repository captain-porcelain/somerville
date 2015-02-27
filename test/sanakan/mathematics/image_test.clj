(ns sanakan.mathematics.image-test
  (:require
    [sanakan.mathematics.image :as i]
    [sanakan.mathematics.flood-fill :as ff]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.color.color :as c])
  (:use midje.sweet))

(def image (i/load-image "./resources/test-image.jpg"))

(defn decider-fn
  [p1 p2]
  (let [vfn (fn [p] (c/rgba (.getRGB image (:x p) (:y p))))
        cie (c/cie76 (vfn p1) (vfn p2))]
    (< cie 3)))

;(dorun (println (decider-fn (p/point 0 0) (p/point 1 1))))

(def numbers (take 320 (iterate inc 0)))
(def points (for [a numbers
                  b numbers]
              (p/point a b)))

;(dorun (println (str (java.util.Date.) " start")))
;(def partitions (try (ff/partition2 points decider-fn 0 0 319 319) (catch Exception e (.printStackTrace e))))
;(dorun (println (str (java.util.Date.) " end")))


