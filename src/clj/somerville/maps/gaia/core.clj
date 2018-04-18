;; http://experilous.com/1/blog/post/procedural-planet-generation
;; https://github.com/Engelberg/ubergraph
(ns somerville.maps.gaia.core
  (:require
    [clojure.set :as s]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line])
  (:import java.util.Random))

(defrecord Node [data neighbors])

(def icosahedron-corners
  (list
    (point/point -0.26286500 0.0000000 0.42532500)
    (point/point 0.26286500 0.0000000 0.42532500)
    (point/point -0.26286500 0.0000000 -0.42532500)
    (point/point 0.26286500 0.0000000 -0.42532500)
    (point/point 0.0000000 0.42532500 0.26286500)
    (point/point 0.0000000 0.42532500 -0.26286500)
    (point/point 0.0000000 -0.42532500 0.26286500)
    (point/point 0.0000000 -0.42532500 -0.26286500)
    (point/point 0.42532500 0.26286500 0.0000000)
    (point/point -0.42532500 0.26286500 0.0000000)
    (point/point 0.42532500 -0.26286500 0.0000000)
    (point/point -0.42532500 -0.26286500 0.0000000)))

(def cube-corners
  (list
    (point/point -1 -1 -1)
    (point/point -1 -1  1)
    (point/point -1  1 -1)
    (point/point -1  1  1)
    (point/point  1 -1 -1)
    (point/point  1 -1  1)
    (point/point  1  1 -1)
    (point/point  1  1  1)))

(defn closest
  "Find the points in ps that are closest to a point p."
  [p ps]
  (let [distances (rest (sort-by first (map #(vector (point/distance p %) %) ps)))
        ref-dist (first (first distances))]
    (map #(apply line/line (sort (list p (second %)))) (take-while #(gcommons/close-to ref-dist (first %)) distances))))
    ;(map #(apply line/line (sort (list p (second %)))) (take 5 distances))))

(defn create-net
  "Create lines from a set of points. Each point is connected to all those that are closest to it."
  [points]
  (distinct (sort (reduce concat (map #(closest % points) points)))))

(defn triangles
  "Create a set of triangles from a set of lines. If two lines share a point create the triangle from
  first point to shared to last point and thus back to first."
  [lines]
  (filter #(not (nil? %))
    (for [l1 lines
          l2 lines]
      (when (not= l1 l2)
        (when (= (:p2 l1) (:p1 l2))
          (polygon/from-points (list (:p1 l1) (:p2 l1) (:p2 l2))))))))

(defn icosahedron
  "Create a set of triangles that represent an icosahedron."
  [scale]
  (triangles (map #(line/scale % scale) (create-net icosahedron-corners))))

(defn cube
  "Create a list of lines that represent a cube."
  [scale]
  (triangles (map #(line/scale % scale) (create-net cube-corners))))

(defn slerp
  "Spheric linear interpolation between two points at parameter t."
  [p1 p2 t]
  (let [np1 (point/normalize p1)
        np2 (point/normalize p2)
        dot (point/dot np1 np2)
        dot (if (< dot 0) (* -1 dot) dot)
        np1 (if (< dot 0) (point/scale np1 -1) np1)
        theta0 (Math/acos dot)
        theta (* t theta0)
        s0 (- (Math/cos theta) (* dot (/ (Math/sin theta) (Math/sin theta0))))
        s1 (/ (Math/sin theta) (Math/sin theta0))]
    (point/add (point/scale p1 s0) (point/scale p2 s1))))

(defn subdivide-lines
  "Take a list of lines half each and create lines for each of the resulting points to its closest neighbors."
  [lines]
  (let [midpoints (map #(slerp (:p1 %) (:p2 %) 0.5) lines)
        points (apply s/union (map #(hash-set (:p1 %) (:p2 %)) lines))]
    (create-net (s/union (into #{} midpoints) points))))

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
