;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery-test
  (:require
    [somerville.image :as image]
    [somerville.dungeons.discovery :as discovery]
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
  (let [parsed (discovery/parse wall-description)]
    (is (= 2 (count parsed)))
    (is (= (l/line (p/point 10 10) (p/point 30 10)) (nth parsed 0)))
    (is (= (l/line (p/point 30 10) (p/point 30 30)) (nth parsed 1)))))

(defn run-manual-test
  []
  (let [walls-1 "line   0,100  50,100
                 line  75,100 200,100
                 line 200,0   200,100
                 line 100,0   100,25
                 line 100,50  100,100

                 line   0,125 200,125
                 line   0,250 200,250
                 line 200,125 200,175
                 line 200,200 200,250

                 line 150,200 150,375
                 line 200,250 200,325
                 line 200,350 200,400

                 line 225,0   225,375
                 line 300,0   300,50
                 line 300,75  300,175
                 line 300,200 300,300
                 line 300,325 300,375

                 line 225,125 300,125
                 line 225,250 300,250
                 line 225,375 300,375

                 line 325,0   325,50
                 line 325,75  325,175
                 line 325,200 325,300
                 line 325,325 325,375

                 line 325,125 400,125
                 line 325,250 400,250
                 line 325,375 400,375"
        points-1 (list [10 110] [100 120] [190 115] [65 95])
        i (discovery/discover points-1 walls-1 400 400 100)]
    (image/write-image "/tmp/discovery.png" i)))

;(run-manual-test)
