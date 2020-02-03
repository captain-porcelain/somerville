(ns somerville.color.color)

(defrecord ColorRGBA [r g b a])
(defrecord ColorLab [l a b])
(defrecord ColorXYZ [x y z])

(defn random-color
  "Create a random color without transparency."
  []
  (ColorRGBA. (rand-int 255) (rand-int 255) (rand-int 255) 255))

(defn convert-bit
  #?(:clj [^Integer c]
     :cljs [c])
  (ColorRGBA. (bit-shift-right (bit-and c 0xff0000) 16) (bit-shift-right (bit-and c 0xff00) 8) (bit-and c 0xff)  (bit-shift-right (bit-and c 0xff000000) 24)))

(defn rgba
  "Convert integer to rgb tupel. No transparancy is supported."
  ([c]
    (convert-bit c))
  ([r g b]
   (ColorRGBA. r g b 255))
  ([r g b a]
   (ColorRGBA. r g b a)))

(def white-d50 (ColorXYZ. 0.964221 1.0 0.825211))

(defn- xyzconv1
  [c]
  (if (<= c 0.040) (/ c 12) (Math/pow (/ (+ c 0.055) 1.055) 2.4)))

(defn- xyzconv2
  [c]
  (let [eps (/ 216.0 24389.0)
        k (/ 24389.0 27.0)]
    (if (> c eps) (Math/pow c (/ 1 3)) (/ (+ (* k c) 16.0) 116.0))))

(defn xyz
  "Convert RGBA to XYZ. Found at http://stackoverflow.com/questions/4593469/java-how-to-convert-rgb-color-to-cie-lab"
  [rgb]
  (let [r (xyzconv1 (/ (:r rgb) 255.0))
        g (xyzconv1 (/ (:g rgb) 255.0))
        b (xyzconv1 (/ (:b rgb) 255.0))
        Xr 0.964221
        Yr 1.0
        Zr 0.825211
        X (+ (* 0.436052025 r) (* 0.385081593 g) (+ 0.143087414 b))
        Y (+ (* 0.222491598 r) (* 0.71688606  g) (+ 0.060621486 b))
        Z (+ (* 0.013929122 r) (* 0.097097002 g) (+ 0.71418547  b))
        xr (/ X Xr)
        yr (/ Y Yr)
        zr (/ Z Zr)]
    (ColorXYZ. xr yr zr)))

(defn lab
  "Convert a rgb color to Lab colorspace."
  [rgb]
  (let [cxyz (xyz rgb)
        fx (xyzconv2 (:x cxyz))
        fy (xyzconv2 (:y cxyz))
        fz (xyzconv2 (:z cxyz))
        Ls (- (* 116 fy) 16)
        as (* 500 (- fx fy))
        bs (* 200 (- fy fz))]
    (ColorLab. (int (+ (* 2.55 Ls) 0.5)) (int (+ as 0.5)) (int (+ bs 0.5)))))

(def justnoticeable 2.3)

(defn cie76
  "Calculate color difference between two colors."
  [c1 c2]
  (let [lab1 (lab c1)
        lab2 (lab c2)
        ld (- (:l lab2) (:l lab1))
        ad (- (:a lab2) (:a lab1))
        bd (- (:b lab2) (:b lab1))]
    (Math/sqrt (+ (* ld ld) (+ ad ad) (* bd bd)))))

(defn to-vector
  "Convert record to vector"
  [c]
  [(:r c) (:g c) (:b c) (:a c)])

(defn interpolate-rgb
  "Interpolate between two colors in RGBA space."
  [c1 c2 t]
  (rgba
    (int (+ (:r c1) (* t (- (:r c2) (:r c1)))))
    (int (+ (:g c1) (* t (- (:g c2) (:g c1)))))
    (int (+ (:b c1) (* t (- (:b c2) (:b c1)))))
    (int (+ (:a c1) (* t (- (:a c2) (:a c1)))))))

(defn color-steps
  "Create interpolated colors between c1 and c2."
  [c1 c2 steps]
  (map #(interpolate-rgb c1 c2 %) (map #(/ % (dec steps)) (range steps))))

(defn same?
  [c1 c2]
  (and
    (= (:r c1) (:r c2))
    (= (:g c1) (:g c2))
    (= (:b c1) (:b c2))
    (= (:a c1) (:a c2))))

(def format-fn
  #?(:clj format
     :cljs goog.string/format))

(defn rgba->hex
  "Convert an rgba color to hex string."
  [c]
  (str "#" (format-fn "%02x" (:r c)) (format-fn "%02x" (:g c)) (format-fn "%02x" (:b c)) (format-fn "%02x" (:a c))))

