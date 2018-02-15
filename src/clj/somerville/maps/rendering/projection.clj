;; Various projection algorithms but none do a very good job.
(ns somerville.maps.rendering.projection
  (:import
    [org.apache.commons.math3.geometry.euclidean.threed Plane Vector3D])
  (:require
    [somerville.geometry.point :as p]))

;;=======================================================================================================================
;; Calculating projections based on Apache Commons Math

(defn make-projector
  [plane-x plane-y]
  (let [plx (Plane. (Vector3D. (:x plane-x) (:y plane-x) (:z plane-x)))
        ply (Plane. (Vector3D. (:x plane-y) (:y plane-y) (:z plane-y)))]
    (fn [p] (let [v (Vector3D. (:x p) (:y p) (:z p))]
              (p/point (.getOffset plx v) (.getOffset ply v))))))

;;=======================================================================================================================
;; Calculating projections based on
;; http://www.codeincodeblock.com/2012/03/projecting-3d-world-co-ordinates-into.html

(defrecord Camera [from to up angleh anglev zoom front back projection])

(defn camera
  ([from to up]
   (Camera. from to up 45.0 45.0 1.0 1.0 200.0 0))
  ([]
   (camera (p/point 0 -50 0) (p/point 0 50 0) (p/point 0 0 1))))

(defrecord Screen [center width height])

(defn screen
  [c w h]
  (Screen. c w h))

(defn basis-vectors
  [camera]
  (let [basis-b (p/normalize (p/subtract (:to camera) (:from camera)))
        basis-a (p/normalize (p/cross (:up camera) basis-b))
        basis-c (p/cross basis-b basis-a)]
    {:basis-a basis-a :basis-b basis-b :basis-c basis-c}))

(defn transpose-world-to-eye
  [w camera basis]
  (let [w2 (p/subtract w (:from camera))
        ex (+ (* (:x w2) (:x (:basis-a basis))) (* (:y w2) (:y (:basis-a basis))) (* (:z w2) (:z (:basis-a basis))))
        ey (+ (* (:x w2) (:x (:basis-b basis))) (* (:y w2) (:y (:basis-b basis))) (* (:z w2) (:z (:basis-b basis))))
        ez (+ (* (:x w2) (:x (:basis-c basis))) (* (:y w2) (:y (:basis-c basis))) (* (:z w2) (:z (:basis-c basis))))]
    (p/point ex ey ez)))

(defn transpose-eye-to-norm
  [e camera apertures]
  (if (= 0 (:projection camera))
    (let [d (/ (:zoom camera) (:y e))
          nx (* d (/ (:x e) (:h apertures)))
          ny (:y e)
          nz (* d (/ (:z e) (:v apertures)))]
      (p/normalize (p/point nx ny nz)))
    (let [nx (* (:zoom camera) (/ (:x e) (:h apertures)))
          ny (:y e)
          nz (* (:zoom camera) (/ (:z e) (:v apertures)))]
      (p/normalize (p/point nx ny nz)))))

(defn transpose-norm-to-screen
  [n screen]
  (p/point
    (int (- (:x (:center screen)) (/ (* (:width screen) (:x n)) 2)))
    (int (- (:y (:center screen)) (/ (* (:height screen) (:z n)) 2)))))

(defn transpose-world-to-screen
  [w camera screen apertures basis]
  (transpose-norm-to-screen (transpose-eye-to-norm (transpose-world-to-eye w camera basis) camera apertures) screen))

(defn to-rad
  [a]
  (* 2 Math/PI (/ a 360)))

(defn aperture
  [camera]
  {:h (Math/tan (to-rad (* (:angleh camera) (/ 0.01745329252 2))))
   :v (Math/tan (to-rad (* (:anglev camera) (/ 0.01745329252 2))))})

(defn project-1
  [size width height x y z]
  (let [camera (camera (p/point 0 0 200) (p/point (/ size 2) (/ size 2) 0) (p/point 0 0 1))
        screen (Screen. (p/point (/ width 2) (/ height 2)) width height)
        apertures (aperture camera)
        basis (basis-vectors camera)
        p3d (p/point x y z)]
    (transpose-world-to-screen p3d camera screen apertures basis)))

;;=======================================================================================================================
;; Calculating projections based on a short answer from
;; https://stackoverflow.com/questions/724219/how-to-convert-a-3d-point-into-2d-perspective-projection

(defn project-wtf
  [size width height x y z]
  (let [cx (int (/ size 2))
        cy (int (/ size 2))
        cz size
        z (if (= 0 z) 1 z)
        f (- z cz)
        px (+ cx (* (- x cx) (/ f z)))
        py (+ cy (* (- y cy) (/ f z)))]
    (p/point px py)))

;;=======================================================================================================================
;; Calculating projections based on
;; http://www.playfuljs.com/realistic-terrain-in-130-lines/<Paste>

(defn iso-projection
  [size x y]
  (p/point (* 0.5 (- (+ size x) y)) (* 0.5 (+ x y))))

(defn project-3
  [size width height x y z]
  (let [point (iso-projection size x y)
        x0    (* width 0.5)
        y0    (* height 0.2)
        x     (* 6 (- (:x point) (* size 0.5)))
        y     (inc (* 0.005 (- size (:y point))))
        z     (- (* size 3) (+ (* 0.4 z) (* (:y point) 0.05)))]
    (p/point (+ x0 (/ x y)) (+ y0 (/ z y)))))


