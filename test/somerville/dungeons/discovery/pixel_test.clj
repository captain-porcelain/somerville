;; Test the rasterize functions.
(ns somerville.dungeons.discovery.pixel-test
  (:require
    [somerville.dungeons.discovery.rasterize :as rasterize]
    [somerville.image :as image]
    [fs.core :as fs])
  (:use clojure.test)
  (:use somerville.dungeons.discovery.pixel))

(deftest discovered-points
  (def dis (create-undiscovered 640 400))
  (def tstart (System/currentTimeMillis))
  (def sightlines (rasterize/translate-lines (rasterize/sight-lines 100) [100 100]))
  (def wall (image/load-image "./test-resources/dungeon-wallmap.png"))
  (def filtered (map #(filter-line wall %) sightlines))
  (is (= (.getWidth dis) 640))
  (is (= (.getWidth wall) 640))
  (update-discovered dis filtered)
  (is (= (.getWidth dis) 640))
  (def tend (System/currentTimeMillis))
  ;(dorun (println (str "time for discovery of 1 point: " (- tend tstart) "ms")))
  (image/write-image "/tmp/dis1.png" dis))

(defn test-points
  [wallmap points visualrange]
  (let [tstart (System/currentTimeMillis)
        dis2 (discover-list wallmap points visualrange)
        tend (System/currentTimeMillis)
        tmp (dorun (println (str "time for discovery of " (count points) " points: " (- tend tstart) "ms")))]
    (image/write-image (str "/tmp/dis-" (count points) ".png") dis2)))

(def points1 (list [30 200]))
;(test-points "dungeon-wallmap.png" points1 100)

(def points2 (list [30 200] [150 210]))
;(test-points "dungeon-wallmap.png" points2 100)

(def points3 (list [30 200] [150 210] [550 200]))
;(test-points "dungeon-wallmap.png" points3 100)

(def points1 (list [130 200]))
(time (image/write-image "/tmp/discovered.png" (discover "./test-resources/dungeon-wallmap.png" points1 100)))
