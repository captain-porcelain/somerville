(ns somerville.maps.gaia.ariadne
  (:require
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as point])
  (:import java.util.Random))

;;=================================================================================================================
;; Taken from Ariadne

(def rnd (new Random))
(def tau (* 2 Math/PI))

(defn- fractions
  "The fractions from 0/n to n/n."
  [n]
  (map #(/ % n) (range 0 (+ 1 n))))

(defn- angles
  "Fractions of a full circle."
  [n]
  (map #(* tau %) (fractions n)))

(defn- angles-half
  "n/2 fractions of a half circle."
  [n]
  (map #(/ (* tau %) 2) (fractions (/ n 2))))

(defn- sphere-angles
  "Angle pairs that describe a full sphere."
  [n]
  (for [a (angles-half n)
        b (drop-last 1 (angles n))]
    (vector a b)))

(defn- randomize
  "Randomize an angle such that it won't conflict with it's neighbours."
  [angle n]
  (let [random (/ (* (/ tau n) (- (.nextFloat rnd) 1)) 1.5)]
    (+ angle random)))

(defn- randomize-angles
  "Randomize all angle pairs in a list"
  [angles n]
  (for [angle angles]
    (vector (randomize (first angle) n) (randomize (second angle) n))))

(defn- randomize-sphere-angles
  "Randomize the angle pairs in a list but keep the poles intact."
  [angles n]
  (let [northpole (take (+ n 1) angles)
        others (drop (+ n 1) (drop-last (+ n 1) angles))
        southpole (drop (- (count angles) (+ n 1)) angles)]
    (concat northpole (randomize-angles others n) southpole)))

(defn sphere-point
  "Create 3 point in 3d that lies on a sphere from the radius and two angles"
  [r a b]
  (let [x (* r (Math/sin a) (Math/cos b))
        y (* r (Math/sin a) (Math/sin b))
        z (* r (Math/cos a))]
    (point/point x y z)))

(defn- sphere-points-from-angles
  "a list of points describing a sphere with radius r and n segments"
  [angles r]
  (for [angle angles]
    (let [a (first angle)
          b (second angle)]
      (sphere-point r a b))))

(defn- sphere-points
  "a list of points describing a sphere with radius r and n segments"
  [r n]
  ;(let [angles (randomize-sphere-angles (sphere-angles n) n)]
  (let [angles (sphere-angles n)]
    (sphere-points-from-angles angles r)))

(defn- get-next-area
  "create a rectangle from 4 points with a random color"
  [radius points n p q]
  (let [is-end (= (mod (count points) n) 1)
        p1 (nth points 0)
        p2 (if is-end p (nth points 1))
        p3 (if is-end q (nth points (+ n 1)))
        p4 (nth points n)]
    (polygon/from-points (list p1 p2 p3 p4))))

(defn- sphere-areas-impl
  "keeps track of the two points making up the start of a segment"
  [radius rectangles points n p1 p2]
  (let [is-start (= (mod (count points) n) 0)
        p1 (if is-start (nth points 0) p1)
        p2 (if is-start (nth points n) p2)]
    (if (< (count points) (+ 2 n))
      (conj rectangles (get-next-area radius points n p1 p2))
      (recur radius (conj rectangles (get-next-area radius points n p1 p2)) (rest points) n p1 p2))))

(defn- sphere-areas
  "create the areas from a list of points"
  [radius points n]
  (sphere-areas-impl radius [] points n (nth points 0) (nth points n)))

(defn random-areas
  "Create a random world of size radius r and split in to segments."
  [radius segments]
  (sphere-areas radius (sphere-points radius segments) segments))

(defn random-area-lines
  "Create a random world of size radius r and split in to segments."
  [radius segments]
  (reduce concat (map :lines (random-areas radius segments))))
