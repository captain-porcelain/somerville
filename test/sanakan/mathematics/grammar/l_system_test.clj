(ns sanakan.mathematics.grammar.l-system-test
  (:require
    [sanakan.mathematics.grammar.l-system :as ls])
  (:use midje.sweet))

;; test the algae system
(def algae-rule1 (ls/rule :a '(:a :b)))
(def algae-rule2 (ls/rule :b '(:a)))
(def algae-rules (list algae-rule1 algae-rule2))
(def algae (ls/lsystem :a algae-rules))
(fact (:state algae) => '(:a))
(def algae-product-1 (ls/produce algae))
(fact (:state algae-product-1) => '(:a :b))
(def algae-product-2 (ls/produce algae-product-1))
(fact (:state algae-product-2) => '(:a :b :a))
(def algae-product-3 (ls/produce algae-product-2))
(fact (:state algae-product-3) => '(:a :b :a :a :b))

;; test the Koch curve
(def koch-rule1 (ls/rule :F '(:F :+ :F :- :F :- :F :+ :F)))
(def koch-rules (list koch-rule1))
(def koch (ls/lsystem :F koch-rules))
(fact (:state koch) => '(:F))
(def koch-product-1 (ls/produce koch))
(fact (:state koch-product-1) => '(:F :+ :F :- :F :- :F :+ :F))
(def koch-product-2 (ls/produce koch-product-1))
(fact (:state koch-product-2) => '(:F :+ :F :- :F :- :F :+ :F :+ :F :+ :F :- :F :- :F
                                      :+ :F :- :F :+ :F :- :F :- :F :+ :F :- :F :+ :F
                                      :- :F :- :F :+ :F :+ :F :+ :F :- :F :- :F :+ :F))
