(ns somerville.maps.dungeons.rendering.ascii
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.maps.grid :as grid]))

;==================================================================================================================
; General printing of grids

(defn print-walker
  "Print every element in the grid."
  [g e]
  (dorun (println (str "Element at " (p/x e) ", " (p/y e) ": " e))))

;==================================================================================================================
; Generate ascii representation of rectangular grids

(defn wall-top?
  [g w h e]
  (or
    (= 0 h)
    (and (:masked e) (not (:masked (grid/get-from g w (dec h)))))
    (and (not (:masked e)) (:masked (grid/get-from g w (dec h))))
    (and (not (:masked e)) (not (commons/in? :north (:links e))))))

(defn wall-left?
  [g w h e]
  (or
    (= 0 w)
    (and (:masked e) (not (:masked (grid/get-from g (dec w) h))))
    (and (not (:masked e)) (:masked (grid/get-from g (dec w) h)))
    (and (not (:masked e)) (not (commons/in? :west (:links e))))))

(defn wall-right?
  [g w h e]
  (or
    (= (dec (:width g)) w)))

(defn line1
  [g e]
  (str
    "+"
    (if (wall-top? g (p/x e) (p/y e) e) "---" "   ")
    (when (wall-right? g (p/x e) (p/y e) e) "+")))

(defn line2
  [g e print-key]
  (str
    (if (wall-left? g (p/x e) (p/y e) e) "|" " ")
    (cond
      (:masked e) " x "
      (and (not (nil? print-key)) (not (nil? (print-key e)))) (if (< (print-key e) 10) (str " " (print-key e) " ") (str " " (print-key e) ""))
      :else "   ")
    (when (wall-right? g (p/x e) (p/y e) e) "|")))

(defn last-line
  [g]
  (str (apply str (repeatedly (:width g) #(str "+---"))) "+"))

(defn make-ascii-walker
  "Associated ascii art to each cell."
  [print-key]
  (fn [g e] (grid/set-in g (p/x e) (p/y e) (assoc e :ascii-1 (line1 g e) :ascii-2 (line2 g e print-key)))))

(defn ascii-row
  "Create partial ASCII representation for one row in the grid."
  [g y]
  (let [a (for [x (range (:width g))]
            [(:ascii-1 (grid/get-from g x y)) (:ascii-2 (grid/get-from g x y))])]
    (str (reduce str (map first a)) "\n" (reduce str (map second a)))))

(defn ascii-rect
  "Create ASCII representation for a grid."
  [g print-key]
  (let [g2 (grid/walk g (make-ascii-walker print-key))
        lines (for [y (range (:height g2))] (ascii-row g2 y))]
    (str
      (clojure.string/join "\n" lines)
      "\n"
      (clojure.string/join "" (last-line g2)))))

;==================================================================================================================
; Generate ascii representation of hexagonal grids
; Example for hex grids:
;
;   /‾\     line a
;/‾\\ //‾\  line b
;\_//‾\\_/  line a
;/‾\\_//‾\  line b
;\_//‾\\_/  line a
;   \_/     line b


(defn upper-hex
  [g c print-key]
  (if (or (= {} c) (= 0 (count (:links c))))
    "    "
    (str (if (commons/in? :north-west (:links c)) " " "/")
         (if
           (commons/in? :north      (:links c))
           (if (nil? (print-key c)) "  " (if (< 9 (print-key c)) (print-key c) (str " " (print-key c))))
           (if (nil? (print-key c)) "‾‾" (if (< 9 (print-key c)) (print-key c) (str "‾" (print-key c)))))
         (if (commons/in? :north-east (:links c)) " " "\\"))))

(defn lower-hex
  [g c]
  (if (or (= {} c) (= 0 (count (:links c))))
    "    "
    (str (if (commons/in? :south-west (:links c)) " " "\\")
         (if (commons/in? :south      (:links c)) "  " "__")
         (if (commons/in? :south-east (:links c)) " " "/"))))

(defn hex-line-a
  [g y print-key]
  (clojure.string/join ""
    (for [x (range (:width g))]
      (str (if (= 0 (mod x 2))
             (lower-hex g (grid/get-from g x (dec y)))
             (upper-hex g (grid/get-from g x y) print-key))))))

(defn hex-line-b
  [g y print-key]
  (clojure.string/join ""
    (for [x (range (:width g))]
      (str (if (= 0 (mod x 2))
             (upper-hex g (grid/get-from g x y) print-key)
             (lower-hex g (grid/get-from g x y)))))))

(defn ascii-hex
  "Create ASCII representation for a grid."
  [g print-key]
  (clojure.string/join "\n"
    (for [y (range (inc (:height g)))]
      (str (hex-line-a g y print-key) "\n" (hex-line-b g y print-key)))))

;==================================================================================================================
; Printing integer grids

(defn max-length
  [g]
  (apply max
    (for [y (range (:height g))
          x (range (:width g))]
      (count (str (grid/get-from g x y))))))

(defn whitespace-pad
  [s l]
  (cond
    (> l 1) (whitespace-pad (str " " s " ") (- l 2))
    (= 1 l) (str " " s)
    :else s))

(defn ascii-ints
  [g]
  (let [l (inc (max-length g))]
    (clojure.string/join "\n"
      (for [y (range (:height g))]
        (clojure.string/join " "
          (for [x (range (:width g))]
            (whitespace-pad (grid/get-from g x y) (- l (count (str (grid/get-from g x y)))))))))))

;==================================================================================================================
; General printing of ascii art

(defn out
  ([g]
   (out g nil))
  ([g print-key]
   (dorun
     (println
       (case (:data-type g)
         :int (ascii-ints g)
         :map (case (:grid-type g)
                :rect (ascii-rect g print-key)
                :hex  (ascii-hex  g print-key)))))))

