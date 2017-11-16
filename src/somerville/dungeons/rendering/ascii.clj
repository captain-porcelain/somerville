(ns somerville.dungeons.rendering.ascii
  (:require
    [somerville.commons :as commons]
    [somerville.dungeons.generators.grid :as grid]))

;==================================================================================================================
; General printing of grids

(defn print-walker
  "Print every element in the grid."
  [g e]
  (dorun (println (str "Element at " (:x e) ", " (:y e) ": " e))))

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
    (if (wall-top? g (:x e) (:y e) e) "---" "   ")
    (when (wall-right? g (:x e) (:y e) e) "+")))

(defn line2
  [g e print-gold]
  (str
    (if (wall-left? g (:x e) (:y e) e) "|" " ")
    (cond
      (:masked e) " x "
      (and print-gold (not (nil? (:gold e)))) (if (< (:gold e) 10) (str " " (:gold e) " ") (str " " (:gold e) ""))
      :else "   ")
    (when (wall-right? g (:x e) (:y e) e) "|")))

(defn last-line-old
  [g]
  (for [i (range (:width g))]
    (when-not (:masked (grid/get-from g i (dec (:height g))))
      (str "+" "---" (when (= i (dec (:width g))) "+")))))

(defn last-line
  [g]
  (str (apply str (repeatedly (:width g) #(str "+---"))) "+"))

(defn make-ascii-walker
  "Associated ascii art to each cell."
  [print-gold]
  (fn [g e] (grid/set-in g (:x e) (:y e) (assoc e :ascii-1 (line1 g e) :ascii-2 (line2 g e print-gold)))))

(defn ascii-row
  "Create partial ASCII representation for one row in the grid."
  [g y]
  (let [a (for [x (range (:width g))]
            [(:ascii-1 (grid/get-from g x y)) (:ascii-2 (grid/get-from g x y))])]
    (str (reduce str (map first a)) "\n" (reduce str (map second a)))))

(defn ascii-rect
  "Create ASCII representation for a grid."
  [g print-gold]
  (let [g2 (grid/walk g (make-ascii-walker print-gold))
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
  [g c print-gold]
  (if (= {} c)
    "    "
    (str (if (commons/in? :north-west (:links c)) " " "/")
         (if
           (commons/in? :north      (:links c))
           (if (or (not print-gold) (nil? (:gold c))) "  " (if (< 9 (:gold c)) (:gold c) (str " " (:gold c))))
           (if (or (not print-gold) (nil? (:gold c))) "‾‾" (if (< 9 (:gold c)) (:gold c) (str "‾" (:gold c)))))
         (if (commons/in? :north-east (:links c)) " " "\\"))))

(defn lower-hex
  [g c]
  (if (= {} c)
    "    "
    (str (if (commons/in? :south-west (:links c)) " " "\\")
         (if (commons/in? :south      (:links c)) "  " "__")
         (if (commons/in? :south-east (:links c)) " " "/"))))

(defn hex-line-a
  [g y print-gold]
  (clojure.string/join ""
    (for [x (range (:width g))]
      (str (if (= 0 (mod x 2))
             (lower-hex g (grid/get-from g x (dec y)))
             (upper-hex g (grid/get-from g x y) print-gold))))))

(defn hex-line-b
  [g y print-gold]
  (clojure.string/join ""
    (for [x (range (:width g))]
      (str (if (= 0 (mod x 2))
             (upper-hex g (grid/get-from g x y) print-gold)
             (lower-hex g (grid/get-from g x y)))))))

(defn ascii-hex
  "Create ASCII representation for a grid."
  [g print-gold]
  (clojure.string/join "\n"
    (for [y (range (inc (:height g)))]
      (str (hex-line-a g y print-gold) "\n" (hex-line-b g y print-gold)))))

;==================================================================================================================
; General printing of ascii art

(defn out
  ([g]
   (dorun (println
            (case (:grid-type g)
              :rect (ascii-rect g false)
              :hex  (ascii-hex  g false)))))
  ([g print-gold]
   (dorun (println
            (case (:grid-type g)
              :rect (ascii-rect g print-gold)
              :hex  (ascii-hex  g print-gold))))))

