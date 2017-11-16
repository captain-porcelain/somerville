;; Functions to create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery.discover
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line]
    [taoensso.timbre :as log]))

(defn translate-line
  "Translate a line description into an actual line."
  [parameters]
  ;; Check that we have two parameters
  (when (= 2 (count parameters))
    (let [parts (reduce concat (map #(clojure.string/split % #",") parameters))
          iparts (filter #(not (nil? %)) (map #(commons/parse-int % nil) parts))]
      (when (= 4 (count iparts))
        (line/line (point/point (nth iparts 0) (nth iparts 1)) (point/point (nth iparts 2) (nth iparts 3)))))))

(defn translate-description
  "Translate one wall description."
  [^String description]
  (let [parts (clojure.string/split (.trim description) #"\s+")]
    (when (< 1 (count parts))
      (case (first parts)
        "line" (translate-line (rest parts))
        :else nil))))

(defn parse
  "Given a string representation of wall descriptions create the geometrical representations for them."
  [^String wall-description]
  (let [slines (clojure.string/split wall-description #"\n")
        dlines (filter #(not (nil? %)) (map translate-description slines))]
    dlines))
