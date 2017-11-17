;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discover-test
  (:require
    [somerville.dungeons.discovery.discover :as discover]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c])
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
    (is (= (l/line (p/point 10 10) (p/point 20 10)) (nth parsed 0)))
    (is (= (l/line (p/point 20 10) (p/point 20 20)) (nth parsed 1)))))

(deftest flow
  (let [circle (c/circle (p/point 15 15) 10)
        lines (discover/parse wall-description)]
    (dorun (println (discover/intersections circle lines)))))

