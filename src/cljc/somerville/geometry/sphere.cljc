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
  [index samples offset increment]
  (let [y (+ (dec (* index offset)) (/ offset 2))
        r (Math/sqrt (- 1 (Math/pow y 2)))
        phi (* increment (mod (+ index 1) samples))
        x (* r (Math/cos phi))
        z (* r (Math/sin phi))]
    (point/point x y z)))

(defn fibonacci
  "Create a grid of points on a unit sphere.
  See https://stackoverflow.com/questions/9600801/evenly-distributing-n-points-on-a-sphere"
  [samples]
  (let [offset (/ 2 samples)
        increment (* Math/PI (- 3 (Math/sqrt 5)))]
    (map #(fibonacci-point % samples offset increment) (range samples))))

(defn jitter
  "Randomize a point on a unit sphere."
  [p max-angle]
  (let [p0 (point/point 0 0 0)
        px (point/point 1 0 0)
        py (point/point 0 1 0)
        ax (point/angle-dot p0 px p)
        ay (point/angle-dot p0 py p)
        half (/ max-angle 2)
        rax (* half (/ (- (rand-int 100) 50) 100))
        ray (* half (/ (- (rand-int 100) 50) 100))
        nax (+ ax rax)
        nay (+ ay ray)
        x (+ (:x p) (* dist (Math/cos nax)))
        y (+ (:y p) (* dist (Math/sin nax)))
        z (+ (:z p) (* dist (Math/sin nay)))
        ]
   (point/point x y z)))

