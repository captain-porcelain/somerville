(ns somerville.fills.convex-hull-test
  (:require
    [somerville.fills.convex-hull :as ch]
    [somerville.geometry.commons :as c]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p])
  (:use [clojure.test]))

(def p1  (p/point -1  1))
(def p2  (p/point  0  1))
(def p3  (p/point  1  1))
(def p4  (p/point -2  0))
(def p5  (p/point -1  0))
(def p6  (p/point  0  0))
(def p7  (p/point  1  0))
(def p8  (p/point  2  0))
(def p9  (p/point -1 -1))
(def p10 (p/point  0 -1))
(def p11 (p/point  1 -1))

(def points (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11))

(deftest cut-line
  (let [cl (ch/cut-line points)]
    (is (= (:p1 cl) p4))
    (is (= (:p2 cl) p8))))

(deftest line-partition
  (let [cl (ch/cut-line points)
        ps (ch/partition-points points cl)]
    (is (= 2 (count ps)))
    (is (= 3 (count (:above ps))))
    (is (= #{p1 p2 p3} (into #{} (:above ps))))
    (is (= 3 (count (:below ps))))
    (is (= #{p9 p10 p11} (into #{} (:below ps))))))

(deftest furthest
  (let [cl (ch/cut-line points)
        ps (ch/partition-points points cl)
        fa (ch/furthest-point cl (:above ps))
        fb (ch/furthest-point cl (:below ps))]
    (is (= p3  fa))
    (is (= p11 fb))))

(deftest triangle-filter
  (let [cl (ch/cut-line points)
        ps (ch/partition-points points cl)
        fa (ch/furthest-point cl (:above ps))
        pra (ch/filter-inside-triangle (:above ps) fa)
        fb (ch/furthest-point cl (:below ps))
        prb (ch/filter-inside-triangle (:below ps) fb)]
    (is (= 3 (count pra)))
    (is (= #{p1 p2 p3} (into #{} pra)))
    (is (= 3 (count prb)))
    (is (= #{p9 p10 p11} (into #{} prb)))))

(deftest splitting
  (let [cl (ch/cut-line points)
        pa (:above (ch/partition-points points cl))
        sa (ch/process-side (ch/hull-segment cl pa :above))
        pb (:below (ch/partition-points points cl))
        sb (ch/process-side (ch/hull-segment (l/line (:p2 cl) (:p1 cl)) pb :below)) ]
    (dorun (println "Above:"))
    (dorun (map #(println (c/out %)) sa))
    (dorun (println "Below:"))
    (dorun (map #(println (c/out %)) sb))
  ))
