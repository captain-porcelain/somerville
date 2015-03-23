(ns sanakan.mathematics.grammar.l-system
  (:require
    [sanakan.mathematics.geometry.line :as l]))

;; Define a formal l-system and rules for it.
(defrecord Lsystem [axiom rules state])
(defrecord Rule [match result])

(defn rule
  "Create a rule."
  [match result]
  (Rule. match result))

(defn lsystem
  "Create a fresh l-system."
  [axiom rules]
  (Lsystem. axiom rules (list axiom)))

(defn matching-rule
  "Find the first matching rule for one symbol."
  [sym rules]
  (first (filter #(= sym (:match %)) rules)))

(defn replace-symbol
  "Apply the replacement defined by the rules for a symbol."
  [sym rules]
  (let [r (matching-rule sym rules)]
    (if (nil? r) (list sym) (:result r))))

(defn apply-rules
  "Apply the rules to all symbols in the system."
  [system]
  (reduce concat (map #(replace-symbol % (:rules system)) (:state system))))

(defn produce
  "Given an l-system produce the next instance by appling the rules."
  [system]
  (Lsystem. (:axiom system) (:rules system) (apply-rules system)))

;; Define an object containing iterated state for rendering
(defrecord RenderState [points angle length])
(defrecord Renderer [point-fn angle-fn])

(defn renderer
  "Create holder for render functions."
  [pfn afn]
  (Renderer. pfn afn))

(defn translate
  "Update the current state of rendering based on the next symbol."
  [render-state sym renderer]
  (RenderState.
    ((:point-fn renderer) render-state sym)
    ((:angle-fn renderer) render-state sym)
    (:length render-state)))

(defn connect-points
  "Connect a list of points into a list of lines from point to point."
  [points]
  (map #(l/line %1 %2) (butlast points) (rest points)))

(defn render-points
  "Given an L system create lines that visualizes it."
  [lsystem start-point length renderer]
  (loop [rs (RenderState. (list start-point) 0 length)
         symbols (:state lsystem)]
    (if (= 0 (count symbols))
      (reverse (:points rs))
      (recur (translate rs (first symbols) renderer) (rest symbols)))))

(defn render
  "Given an L system create lines that visualizes it."
  [lsystem start-point length renderer]
  (connect-points (render-points lsystem start-point length renderer)))
