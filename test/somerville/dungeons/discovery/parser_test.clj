;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.parser-test
  (:require
    [somerville.dungeons.discovery.parser :as parser]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(def wall-description
  "line 10,10 30,10
   blubber
   line 10.1,30 10
   line 10.1,30
   line 30,10 30,30")

(deftest parsing
  (let [parsed (parser/parse wall-description)]
    (is (= 2 (count parsed)))
    (is (= (l/line (p/point 10 10) (p/point 30 10)) (nth parsed 0)))
    (is (= (l/line (p/point 30 10) (p/point 30 30)) (nth parsed 1)))))

