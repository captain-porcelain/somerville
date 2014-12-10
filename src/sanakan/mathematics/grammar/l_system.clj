(ns sanakan.mathematics.grammar.l-system)

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
