;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discover-test
  (:require
    [somerville.image :as image]
    [somerville.dungeons.discovery.discover :as discover]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c])
  (:use clojure.test))

(def wall-description
  "line 10,10 30,10
   blubber
   line 10.1,30 10
   line 10.1,30
   line 30,10 30,30")

(deftest parsing
  (let [parsed (discover/parse wall-description)]
    (is (= 2 (count parsed)))
    (is (= (l/line (p/point 10 10) (p/point 30 10)) (nth parsed 0)))
    (is (= (l/line (p/point 30 10) (p/point 30 30)) (nth parsed 1)))))

(def points-1 (list [130 200]))
(def walls-1 (list))

(time (image/write-image "/tmp/discovered-lines-1.png" (discover/discover points-1 walls-1 640 400 100)))

(def points-2 (list [200 200]))
(def walls-2 (discover/parse "line 0,150 400,150"))
(time (image/write-image "/tmp/discovered-lines-2.png" (discover/discover points-2 walls-2 400 400 100)))

(def points-3 (list [200 200] [10 10] [100 100] [300 300] [10 155]))
(def walls-2 (discover/parse "line 0,150 400,150"))
(try
(time (image/write-image "/tmp/discovered-lines-3.png" (discover/discover points-3 walls-2 400 400 100)))
(catch Exception e (.printStackTrace e)))
