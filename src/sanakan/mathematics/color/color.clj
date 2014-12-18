(ns sanakan.mathematics.color.color
  (:import java.util.Random))

(defrecord ColorRGBA [r g b a])
(defrecord ColorLab [l a b])
(defrecord ColorXYZ [x y z])

(defn random-color
  "Create a random color without transparency."
  []
  (ColorRGBA. (rand-int 255) (rand-int 255) (rand-int 255) 0))

(defn rgba
  "Convert integer to rgb tupel. No transparancy is supported."
  [^Integer c]
  (let [
        c1 (int (/ c 256))
        c2 (int (/ c1 256))
        ;c1 (unchecked-divide c 256)
        ;c2 (unchecked-divide c1 256)
        ]
    (ColorRGBA. (mod c 256) (mod c1 256) (mod c2 256) 0)))

(def white-d50 (ColorXYZ. 0.964221 1.0 0.825211))

(defn xyz
  "Convert RGBA to XYZ."
  [rgb]
  (ColorXYZ. (/ (:r rgb) 255.0) (/ (:g rgb) 255.0) (/ (:b rgb) 255.0)))

(defn lab
  "Convert a rgb color to Lab colorspace."
  [rgb]
  )

