(ns sanakan.mathematics.image-test
  (:require
    [sanakan.mathematics.image :as i]
    [sanakan.mathematics.flood-fill :as ff]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.color.color :as c])
  (:use midje.sweet))

(def image (i/load-image "./resources/test-image.jpg"))

(defn value-fn
  [p]
  (c/rgba (.getRGB image (:x p) (:y p))))

(defn decider-fn
  [v nv]
  (let [cie (c/cie76 (:value v) (:value nv))]
    (< cie 3)))

(dorun (println (value-fn (p/point 0 0))))
(dorun (println (value-fn (p/point 1 1))))

(dorun (println (decider-fn {:value (value-fn (p/point 0 0))} {:value (value-fn (p/point 1 1))})))


