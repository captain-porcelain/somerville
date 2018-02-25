;; Various projection algorithms but none do a very good job.
(ns somerville.geometry.projection
  (:import
    [org.apache.commons.math3.geometry.euclidean.threed Plane Vector3D])
  (:require
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.plane :as pl]
    [somerville.geometry.commons :as c]))

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
  [width height x y z]
  (let [camera (camera (p/point 0 0 200) (p/point 0 0 0) (p/point 0 0 1))
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

;;=======================================================================================================================
;; My own projection
;; First set up the camera at a point and a projection plane.
;; Then intersect the line from the camera to the point that is to be projected onto the plane.
;; Lastly transform the point from the plane into a 2d view.

(defrecord Projector [camera focus up projection-plane screen-center width height]
  c/Printable
  (c/out [this i] (str (c/indent i) "Projector settings\n"
                       "Camera at " (c/out camera) "\n"
                       "Focussing on " (c/out focus) "\n"
                       "Plane up is " (c/out up) "\n"
                       "Projection plane is " (c/out projection-plane) "\n"
                       "Screen center is " (c/out screen-center) "\n"
                       "Screen dimensions are " width "x" height))
  (c/out [this] (c/out this 0)))

(defn projection-plane
  "Calculate the projection plane for the given camera and focus points considering the orientation given by up."
  [camera focus up]
  (let [v1 (p/subtract focus camera)
        v2 (p/add focus up)
        v3 (p/cross v1 v2)]
    (pl/plane focus v2 v3)))

(defn projector
  "Create a projector that looks from the camera point to the focus point and a screen of width x height.
  The orientation of the screen plane is determined by the up vector."
  [camera focus up width height]
  (Projector. camera focus up (projection-plane camera focus up) (p/point (/ width 2) (/ height 2)) width height))

(defn linear-combination
  "Find values a b such that v = a * s + b * t."
  [v s t]
  (let [bupper (- (* (:y v) (:x s)) (* (:x v) (:y s)))
        blower (- (* (:y t) (:x s)) (* (:y s) (:x t)))
        b (/ bupper blower)
        a (/ (- (:x v) (* b (:x t))) (:x s))]
    [a b]))

(defn project
  "Project the given point using the projector settings."
  [projector point]
  ;tmp (dorun (println (str "Plane intersection: " (c/out i))))
  (let [i (pl/intersect (:projection-plane projector) (l/line (:camera projector) point))

        ;; no correction for angle base
        ;vi (p/subtract i (:focus projector))
        ;tmp (dorun (println (str "Intersection vector: " (c/out vi))))
        ;p2d (p/add (:screen-center projector) vi)

        ;; testing cross product magic
        ; get horizontal base line
        h (p/cross (p/subtract (:focus projector) (:camera projector)) (:up projector))
        horizontal-base-line (p/scale (p/normalize h) (/ (:width projector) 2))
        tmp (dorun (println (str "Horizontal base line: " (c/out horizontal-base-line))))
        vertical-base-line (p/scale (p/normalize (:up projector)) (/ (:height projector) 2))
        tmp (dorun (println (str "Vertical base line: " (c/out vertical-base-line))))
        ;upper-right-corner (p/add (p/add (:focus projector) horizontal-base-line) vertical-base-line)
        ;c (pl/intersect (:projection-plane projector) (l/line (:camera projector) (:focus projector)))
        c (:focus projector)
        lower-left-corner (p/subtract (p/subtract c horizontal-base-line) vertical-base-line)
        tmp (dorun (println (str "Lower Left Corner" (c/out lower-left-corner))))
        vi (p/subtract i lower-left-corner)
        tmp (dorun (println (str "Intersection vector: " (c/out vi))))
        ;dvi (p/distance i (:focus projector))
        ;dc (p/distance upper-right-corner (:focus projector))
        ;alpha (Math/cos (/ dvi dc))
        ;p2d (p/point-at (:screen-center projector) alpha dvi)
        p2d vi

        ;tmp (dorun (println (str "Projected point: " (c/out p2d))))
        ]
    p2d))
