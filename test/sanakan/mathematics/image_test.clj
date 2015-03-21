(ns sanakan.mathematics.image-test
  (:require
    [sanakan.mathematics.image :as i]
    [sanakan.mathematics.fills.flood-fill :as ff]
    [sanakan.mathematics.fills.line-fill :as lf]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.color.color :as c])
  (:use midje.sweet))

(def image (i/load-image "./resources/test-image.jpg"))

(defn decider-fn
  [p1 p2]
  (let [vfn (fn [p] (c/rgba (.getRGB image (:x p) (:y p))))
        cie (c/cie76 (vfn p1) (vfn p2))]
    (< cie 20)))

(def numbers (take 320 (iterate inc 0)))
(def points (for [a numbers
                  b numbers]
              (p/point a b)))

(defn parts
  []
  (let [starttime (System/currentTimeMillis)
        partitions (lf/clusters (p/point 0 0) 319 319 decider-fn)
        tmp (dorun partitions)
        endtime (System/currentTimeMillis)
        tmp (dorun (println (str "found " (count partitions) " partitions in " (- endtime starttime) " ms.")))]
    partitions))

;(parts)
