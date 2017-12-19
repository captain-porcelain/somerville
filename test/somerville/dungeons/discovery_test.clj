;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery-test
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon]
    [java.awt.image BufferedImage])
  (:require
    [taoensso.timbre :as log]
    [taoensso.timbre.appenders.core :as appenders]
    [somerville.image :as image]
    [somerville.dungeons.discovery :as discovery]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c])
  (:use clojure.test))

(log/merge-config!
  {:appenders {:spit (appenders/spit-appender {:fname "/tmp/somerville-discovery-test.log"})}})

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

(deftest relevant-walls
  (let [l1 (l/line (p/point -0.5 0) (p/point 0.5 0))
        l2 (l/line (p/point -0.5 0) (p/point 1.5 0))
        l3 (l/line (p/point -1.5 0) (p/point 1.5 0))
        l4 (l/line (p/point -1.5 2) (p/point 1.5 2))
        cp1 (p/point -1  0)
        cp2 (p/point  0 -1)
        cp3 (p/point  1  0)
        cp4 (p/point  0  1)
        walls (list l1 l2 l3 l4)
        relevant-walls (discovery/relevant-walls (p/point 0 0) walls 1 4)]
    (is (= 7 (count relevant-walls)))
    ;(dorun (map #(println (commons/out %)) relevant-walls))
    (is (commons/close-to 0 (p/distance cp1 (:p1 (nth relevant-walls 0)))))
    (is (commons/close-to 0 (p/distance cp2 (:p2 (nth relevant-walls 0)))))
    (is (commons/close-to 0 (p/distance cp2 (:p1 (nth relevant-walls 1)))))
    (is (commons/close-to 0 (p/distance cp3 (:p2 (nth relevant-walls 1)))))
    (is (commons/close-to 0 (p/distance cp3 (:p1 (nth relevant-walls 2)))))
    (is (commons/close-to 0 (p/distance cp4 (:p2 (nth relevant-walls 2)))))
    (is (commons/close-to 0 (p/distance cp4 (:p1 (nth relevant-walls 3)))))
    (is (commons/close-to 0 (p/distance cp1 (:p2 (nth relevant-walls 3)))))
    (is (= l1 (nth relevant-walls 4)))
    (is (commons/close-to 0 (p/distance (p/point -0.5 0) (:p1 (nth relevant-walls 5)))))
    (is (commons/close-to 0 (p/distance (p/point  1   0) (:p2 (nth relevant-walls 5)))))
    (is (commons/close-to 0 (p/distance (p/point -1   0) (:p1 (nth relevant-walls 6)))))
    (is (commons/close-to 0 (p/distance (p/point  1   0) (:p2 (nth relevant-walls 6)))))))

(deftest point-sorting
  (testing "q1 simple"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point  1  1)
          p2 (p/point  2  1)
          l1 (l/line p1 p2)
          s1 (discovery/sort-line-points l1 cp rp)]
      (is (= p1 (:p1 s1)))
      (is (= p2 (:p2 s1)))))
  (testing "q1 reverse"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point  2  1)
          p2 (p/point  1  1)
          l1 (l/line p1 p2)
          s1 (discovery/sort-line-points l1 cp rp)]
      (is (= p2 (:p1 s1)))
      (is (= p1 (:p2 s1)))))
  (testing "q2"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point -2  1)
          p2 (p/point -1  1)
          l1 (l/line p1 p2)
          s1 (discovery/sort-line-points l1 cp rp)]
      (is (= p1 (:p1 s1)))
      (is (= p2 (:p2 s1)))))
  (testing "multiple quadrants"
    (let [cp (p/point  0  0)
          rp (p/point -1  0)
          p1 (p/point -2 -2)
          p2 (p/point  1  1)
          l1 (l/line p1 p2)
          s1 (discovery/sort-line-points l1 cp rp)]
      (is (= p2 (:p1 s1)))
      (is (= p1 (:p2 s1))))))

(def desc "line 150,180 180,180
           line 150,150 150,180
           line 150,150 180,150
           line 180,150 180,180
           line 210,190 230,190
           line 210,190 210,170
           line 210,170 230,170
           line 230,170 230,190
           line 260,170 260,330")

;(time (image/write-image "/tmp/discovery.png" (discovery/discover (list [200 200] [10 10]) desc 500 500 100 true)))

;(image/write-image "/tmp/discovery.png" (discovery/discover (list [200 200]) "line 260,170 260,330" 500 500 100 true))

;(defn debug-baramzigli
  ;[]
  ;(let [desc "line 150,180 180,180
              ;line 150,150 150,180
              ;line 150,150 180,150
              ;line 180,150 180,180
              ;line 210,190 230,190
              ;line 210,190 210,170
              ;line 210,170 230,170
              ;line 230,170 230,190
              ;line 260,170 260,330"
        ;desc "line 260,170 260,330"
        ;walls (discovery/parse desc)
        ;visualrange 100
        ;polygon-steps 8
        ;point (p/point 200 200)
        ;circle (c/circle point visualrange)
        ;ref-point (p/point -1 200)
        ;relevant-walls (discovery/relevant-walls point walls visualrange polygon-steps)
        ;sorted-walls (map #(discovery/sort-line-points % point ref-point) relevant-walls)
        ;events (discovery/gather-events sorted-walls point ref-point)
        ;events (concat events (list (first events)))
        ;[last-point-1 triangles-1 active-walls-1] (discovery/update-triangles point nil (list) (list) (nth events 0))
        ;[last-point-2 triangles-2 active-walls-2] (discovery/update-triangles point last-point-1 triangles-1 active-walls-1 (nth events 1))
        ;[last-point-3 triangles-3 active-walls-3] (discovery/update-triangles point last-point-2 triangles-2 active-walls-2 (nth events 2))
        ;[last-point-4 triangles-4 active-walls-4] (discovery/update-triangles point last-point-3 triangles-3 active-walls-3 (nth events 3))
        ;[last-point-5 triangles-5 active-walls-5] (discovery/update-triangles point last-point-4 triangles-4 active-walls-4 (nth events 4))
        ;[last-point-6 triangles-6 active-walls-6] (discovery/update-triangles point last-point-5 triangles-5 active-walls-5 (nth events 5))
        ;[last-point-7 triangles-7 active-walls-7] (discovery/update-triangles point last-point-6 triangles-6 active-walls-6 (nth events 6))
        ;[last-point-8 triangles-8 active-walls-8] (discovery/update-triangles point last-point-7 triangles-7 active-walls-7 (nth events 7))
        ;[last-point-9 triangles-9 active-walls-9] (discovery/update-triangles point last-point-8 triangles-8 active-walls-8 (nth events 8))
        ;[last-point-10 triangles-10 active-walls-10] (discovery/update-triangles point last-point-9 triangles-9 active-walls-9 (nth events 9))
        ;[last-point-11 triangles-11 active-walls-11] (discovery/update-triangles point last-point-10 triangles-10 active-walls-10 (nth events 10))
        ;]
    ;;(image/write-image "/tmp/discovery.png" (discovery/discover (list [(:x point) (:y point)]) desc 500 500 100))
    ;;(dorun (map (fn [a] (dorun (map #(println (commons/out %)) a))) events))
    ;(discovery/debug-draw "/tmp/step-00.png" 500 500 circle (list) sorted-walls (list) nil (list))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-01.png" 500 500 circle (map :point (nth events 0)) sorted-walls active-walls-1 last-point-1 triangles-1)
    ;(dorun (map #(println (commons/out %)) (nth events 0)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-1))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-1)))
    ;(dorun (map #(println (commons/out %)) triangles-1))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-02.png" 500 500 circle (map :point (nth events 1)) sorted-walls active-walls-2 last-point-2 triangles-2)
    ;(dorun (map #(println (commons/out %)) (nth events 1)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-2))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-2)))
    ;(dorun (map #(println (commons/out %)) triangles-2))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-03.png" 500 500 circle (map :point (nth events 2)) sorted-walls active-walls-3 last-point-3 triangles-3)
    ;(dorun (map #(println (commons/out %)) (nth events 2)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-3))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-3)))
    ;(dorun (map #(println (commons/out %)) triangles-3))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-04.png" 500 500 circle (map :point (nth events 3)) sorted-walls active-walls-4 last-point-4 triangles-4)
    ;(dorun (map #(println (commons/out %)) (nth events 3)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-4))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-4)))
    ;(dorun (map #(println (commons/out %)) triangles-4))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-05.png" 500 500 circle (map :point (nth events 4)) sorted-walls active-walls-5 last-point-5 triangles-5)
    ;(dorun (map #(println (commons/out %)) (nth events 4)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-5))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-5)))
    ;(dorun (map #(println (commons/out %)) triangles-5))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-06.png" 500 500 circle (map :point (nth events 5)) sorted-walls active-walls-6 last-point-6 triangles-6)
    ;(dorun (map #(println (commons/out %)) (nth events 5)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-6))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-6)))
    ;(dorun (map #(println (commons/out %)) triangles-6))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-07.png" 500 500 circle (map :point (nth events 6)) sorted-walls active-walls-7 last-point-7 triangles-7)
    ;(dorun (map #(println (commons/out %)) (nth events 6)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-7))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-7)))
    ;(dorun (map #(println (commons/out %)) triangles-7))
    ;(dorun (println "========================================\nevents"))
    ;(discovery/debug-draw "/tmp/step-08.png" 500 500 circle (map :point (nth events 7)) sorted-walls active-walls-8 last-point-8 triangles-8)
    ;(dorun (map #(println (commons/out %)) (nth events 7)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-8))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-8)))
    ;(dorun (map #(println (commons/out %)) triangles-8))
    ;(discovery/debug-draw "/tmp/step-09.png" 500 500 circle (map :point (nth events 8)) sorted-walls active-walls-9 last-point-9 triangles-9)
    ;(dorun (map #(println (commons/out %)) (nth events 8)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-9))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-9)))
    ;(dorun (map #(println (commons/out %)) triangles-9))
    ;(discovery/debug-draw "/tmp/step-10.png" 500 500 circle (map :point (nth events 9)) sorted-walls active-walls-10 last-point-10 triangles-10)
    ;(dorun (map #(println (commons/out %)) (nth events 9)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-10))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-10)))
    ;(dorun (map #(println (commons/out %)) triangles-10))
    ;(discovery/debug-draw "/tmp/step-11.png" 500 500 circle (map :point (nth events 10)) sorted-walls active-walls-11 last-point-11 triangles-11)
    ;(dorun (map #(println (commons/out %)) (nth events 10)))
    ;(dorun (println "----------------------------------------\nwalls"))
    ;(dorun (map #(println (commons/out %)) active-walls-11))
    ;(dorun (println "----------------------------------------\ntriangles"))
    ;(dorun (println (commons/out last-point-11)))
    ;(dorun (map #(println (commons/out %)) triangles-11))))

;(debug-baramzigli)

(defn run-manual-test
  [walls points width height visualrange]
  (try
    (let [i (discovery/discover points walls width height visualrange true)]
      (image/write-image "/tmp/discovery.png" i))
    (catch Exception e (.printStackTrace e))))

(defn manual-test-rooms
  []
  (let [points '([10 110] [100 120] [190 115] [65 95])
        walls "line   0,100  50,100
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
               line 325,375 400,375"]
    (time (run-manual-test walls points 500 400 100))))

(defn manual-test-baramzigli
  []
  (let [points '([161 1472] [151 1235] [226 996] [515 981] [663 959] [616 883] [806 740] [1065 885] [848 1035] [768 1302] [957 1339])
        points '([151 1235])
        walls "line 1059,1402 1302,1372
               line 552,1297 749,1404
               line 374,1303 552,1297
               line 239,1376 374,1303
               line 881,1350 881,1408
               line 879,1223 881,1294
               line 1242,882 1239,570
               line 1161,879 1242,882
               line 1158,1036 1161,879
               line 926,1036 1158,1036
               line 932,1224 926,1036
               line 1062,1220 802,1224
               line 1061,1405 1062,1220
               line 747,1401 1061,1405
               line 741,1034 747,1401
               line 241,1033 741,1034
               line 239,1375 241,1033
               line 186,1375 239,1375
               line 717,934 716,1036
               line 842,938 845,792
               line 686,941 842,938
               line 463,800 463,943
               line 67,548 76,882
               line 1018,566 1240,573
               line 1034,492 1018,566
               line 955,344 1034,492
               line 871,243 955,344
               line 655,168 871,243
               line 439,231 655,168
               line 325,395 439,231
               line 288,560 325,395
               line 56,555 288,560
               line 0,1373 101,1373
               line 40,940 40,1373
               line 40,940 623,940
               line 168,940 168,868
               line 168,868 75,868"]
    (time (run-manual-test walls points 1300 1517 300))))

(defn manual-test-clear
  []
  (let [points '([500 500])
        walls ""]
    (time (run-manual-test walls points 1000 1000 300))))

(defn manual-test-simple-line
  []
  (let [points '([500 500])
        walls "line 250,750 750,750"]
    (time (run-manual-test walls points 1000 1000 300))))

(defn manual-test-casting
  []
  (let [points '([500 500])
        walls "line 0,750 400,750
               line 600,750 1000,750"]
    (time (run-manual-test walls points 1000 1000 300))))

(manual-test-casting)





