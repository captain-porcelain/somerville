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

(defn test-settings
  [points walls]
  (let [width 400
        height 400
        visualrange 100
        points-by-bounding-boxes (* (count points) (* 4 visualrange visualrange))
        points-by-width-height (* width height)
        tmp (dorun (println "======================================================================="))
        tmp (dorun (println (str "Testing " (count points) " points and " (count walls) " walls:")))
        tmp (dorun (println (str "Pixels by bounding boxes: " points-by-bounding-boxes)))
        tmp (dorun (println (str "Pixels by width and height: " points-by-width-height)))
        discovered-image (discover/create-undiscovered-graphics width height)
        tmp (dorun (println (str "Runtime by bounding boxes:")))
        img-boxed (time (discover/discover-bounding-boxes points walls discovered-image visualrange))
        tmp (image/write-image "/tmp/discovered-lines-boxed.png" img-boxed)
        tmp (dorun (println (str "Runtime by width and height:")))
        img-all (time (discover/discover-all points walls discovered-image visualrange))
        tmp (image/write-image "/tmp/discovered-lines-all.png" img-all)]
    ))

(def points-1 (list [130 200]))
(def walls-1 (list))
;(test-settings points-1 walls-1)


(def points-2 (list [200 200]))
(def walls-2 (discover/parse "line 0,150 400,150"))
;(test-settings points-2 walls-2)


(def points-3 (list [200 200] [10 10] [100 100] [300 300] [10 155]))
(def walls-3 (discover/parse "line 0,150 400,150"))
;(test-settings points-3 walls-3)

(def walls-4
  (discover/parse
    "line 10,10 30,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 30,10 30,30"))
;(test-settings points-2 walls-4)
;(test-settings points-3 walls-4)

(def points-4 (list [200 200] [10 10] [100 100] [300 300] [10 155] [200 200] [10 10] [100 100] [300 300] [10 155]))
;(test-settings points-4 walls-4)

(def walls-5
  (discover/parse
    "line 10,10 30,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 10,30 20,10
     line 100,30 200,10
     line 110,30 210,10
     line 120,30 220,10
     line 130,30 230,10
     line 30,10 30,30"))
;(test-settings points-1 walls-5)
;(test-settings points-4 walls-5)

(def walls-6
  (discover/parse
    "line   0,100  50,100
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
     line 325,375 400,375
    "))

(def points-5 (list [10 110] [100 120] [190 115] [65 95]))
(test-settings points-5 walls-6)


