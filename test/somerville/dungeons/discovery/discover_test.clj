;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discover-test
  (:require
    [somerville.dungeons.discovery.discover :as discover]
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line])
  (:use clojure.test))

(def wall-description
  "line 10,10 20,10
   blubber
   line 10.1,20 10
   line 10.1,20
   line 20,10 20,20")

(deftest parsing
  (let [parsed (discover/parse wall-description)]
    (is (= 2 (count parsed)))
    (is (= (line/line (point/point 10 10) (point/point 20 10)) (nth parsed 0)))
    (is (= (line/line (point/point 20 10) (point/point 20 20)) (nth parsed 1)))))
