(ns somerville.geometry.sphere
  (:require
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as point]
    [taoensso.timbre :as log]))

;; Define a sphere from its center and radius.
(defrecord Sphere [p r]
  commons/Printable
  (commons/out [this i] (str (commons/indent i) "Sphere at " (commons/out p) " with radius " r))
  (commons/out [this] (commons/out this 0)))

(defn sphere
  "Create a sphere from a point and a radius."
  [p r]
  (Sphere. p r))

(defn fibonacci-point
  "Create a fibonacci point on a sphere."
  [index samples offset increment jitter]
  (let [y (+ (dec (* index offset)) (/ offset 2))
        r (Math/sqrt (- 1 (Math/pow y 2)))
        rnd (- 1 (* jitter (/ (rand-int 100) 100)))
        phi (* increment (mod (+ index 1) samples) rnd)
        x (* r (Math/cos phi))
        z (* r (Math/sin phi))]
    (point/point x y z)))

(defn fibonacci
  "Create a grid of points on a unit sphere.
  See https://stackoverflow.com/questions/9600801/evenly-distributing-n-points-on-a-sphere"
  ([samples jitter]
   (let [offset (/ 2 samples)
         increment (* Math/PI (- 3 (Math/sqrt 5)))]
     (map #(fibonacci-point % samples offset increment jitter) (range samples))))
  ([samples]
   (fibonacci samples 0)))

