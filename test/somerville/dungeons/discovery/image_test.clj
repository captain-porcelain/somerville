;; Test functions that create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.image-test
  (:require
    [somerville.image :as images]
    [somerville.dungeons.discovery.ray-cast-wall-trace :as rcwt]
    [somerville.dungeons.discovery.image :as image]
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c])
  (:use clojure.test))

;;==================================================================================================================
;; Manual testing with debuggin output in /tmp/

(defn run-manual-test
  [testname walls points width height visualrange]
  (try
    (let [tmp (reset! rcwt/debug false)
          i (image/discover points walls width height visualrange)]
      (images/write-image (str "/tmp/discovery-" testname ".png") i))
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



