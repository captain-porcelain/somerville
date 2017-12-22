;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discovery-test
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon]
    [java.awt.image BufferedImage])
  (:require
    [taoensso.timbre :as log]
    [taoensso.timbre.appenders.core :as appenders]
    [somerville.image :as image]
    [somerville.dungeons.discovery.discovery :as discovery]
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

(deftest cut-walls
  (let [l1 (l/line (p/point 2 1) (p/point 2 5))
        l2 (l/line (p/point 1 2) (p/point 3 2))
        l3 (l/line (p/point 1 3) (p/point 3 3))
        l4 (l/line (p/point 1 4) (p/point 2 4))
        lines (list l1 l2 l3 l4)
        cuts (discovery/cut-walls lines)]
    (is (= 9 (count cuts)))
    (is (commons/close-to 0 (p/distance (p/point 2 1) (:p1 (nth cuts 0)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p2 (nth cuts 0)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p1 (nth cuts 1)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p2 (nth cuts 1)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p1 (nth cuts 2)))))
    (is (commons/close-to 0 (p/distance (p/point 2 4) (:p2 (nth cuts 2)))))
    (is (commons/close-to 0 (p/distance (p/point 2 4) (:p1 (nth cuts 3)))))
    (is (commons/close-to 0 (p/distance (p/point 2 5) (:p2 (nth cuts 3)))))
    (is (commons/close-to 0 (p/distance (p/point 1 2) (:p1 (nth cuts 4)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p2 (nth cuts 4)))))
    (is (commons/close-to 0 (p/distance (p/point 2 2) (:p1 (nth cuts 5)))))
    (is (commons/close-to 0 (p/distance (p/point 3 2) (:p2 (nth cuts 5)))))
    (is (commons/close-to 0 (p/distance (p/point 1 3) (:p1 (nth cuts 6)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p2 (nth cuts 6)))))
    (is (commons/close-to 0 (p/distance (p/point 2 3) (:p1 (nth cuts 7)))))
    (is (commons/close-to 0 (p/distance (p/point 3 3) (:p2 (nth cuts 7)))))
    (is (commons/close-to 0 (p/distance (p/point 1 4) (:p1 (nth cuts 8)))))
    (is (commons/close-to 0 (p/distance (p/point 2 4) (:p2 (nth cuts 8)))))))

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

;;==================================================================================================================
;; Manual testing with debuggin output in /tmp/

(defn run-manual-test
  [testname walls points width height visualrange]
  (try
    (let [tmp (reset! discovery/debug false)
          i (discovery/discover points walls width height visualrange)]
      (image/write-image (str "/tmp/discovery-" testname ".png") i))
    (catch Exception e (.printStackTrace e))))

(defn manual-test-rooms
  []
  (let [points '([10 110] [100 120] [190 115] [65 95])
        walls (slurp "test-resources/rooms.walls")]
    (time (run-manual-test "rooms" walls points 500 400 100))))

(defn manual-test-baramzigli
  []
  (let [points '([161 1472] [151 1235] [226 996] [515 981] [663 959] [616 883] [806 740] [1065 885] [848 1035] [768 1302] [957 1339])
        walls (slurp "test-resources/baramzigli.walls")]
    (time (run-manual-test "baramzigli" walls points 1300 1517 300))))

(defn manual-test-clear
  []
  (let [points '([500 500])
        walls ""]
    (time (run-manual-test "clear" walls points 1000 1000 300))))

(defn manual-test-simple-line
  []
  (let [points '([500 500])
        walls "line 250,750 750,750"]
    (time (run-manual-test "simple-line" walls points 1000 1000 300))))

(defn manual-test-vertical-parallel-line
  []
  (let [points '([500 500])
        walls "line 500,0 500,400"]
    (time (run-manual-test "vertical-parallel-line" walls points 1000 1000 300))))

(defn manual-test-casting
  []
  (let [points '([500 500])
        walls "line 0,750 400,750
               line 600,750 1000,750"]
    (time (run-manual-test "casting" walls points 1000 1000 300))))

(defn manual-test-maglubiyet
  []
  (let [points '([137 109])
        walls (slurp "test-resources/maglubiyet.walls")]
    (time (run-manual-test "maglubiyet" walls points 6200 5535 300))))
    ;(time (run-manual-test "maglubiyet" walls points 1000 1000 300))))

;(manual-test-clear)
;(manual-test-simple-line)
;(manual-test-vertical-parallel-line)
;(manual-test-casting)
;(manual-test-rooms)
;(manual-test-baramzigli)
(manual-test-maglubiyet)



