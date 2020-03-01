;; Handle parsing image descriptions for inclusion in the sidenotes documentation.
(ns somerville.parsers.doc-image-parser
  (:require
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line]
    [somerville.geometry.circle :as circle]
    [somerville.geometry.arc :as arc]
    [somerville.color.color :as color]
    [clojure.edn :as edn]
    [taoensso.timbre :as log]))


(defn parse-colors
  "Parse a color from the definitions."
  [colors]
  (into {} (map (fn [[k v]] [k (apply color/rgba v)]) colors)))

(defn parse-object
  "Recursively parse objects. Assume that an object is defined as a vector with a keyword and parameters
  which can be translated into function calls."
  [o objects]
  (cond
    (vector? o) (case (first o)
                  :point (apply point/point (map #(parse-object % objects) (rest o)))
                  :line (apply line/line (map #(parse-object % objects) (rest o)))
                  :circle (apply circle/circle (map #(parse-object % objects) (rest o)))
                  :arc (apply arc/arc (map #(parse-object % objects) (rest o)))
                  :arc-lines (apply arc/from-lines (map #(parse-object % objects) (rest o)))
                  o)
    (not (nil? (get objects o))) (get objects o)
    :else o))

(defn parse-objects
  "Parse the object definitions and resolve references."
  [definitions]
  (loop [ds definitions
         objects {}]
    (if (= 0 (count ds))
      objects
      (let [[k o] (first ds)
            po (parse-object o objects)]
        (recur (rest ds) (assoc objects k po))))))

(defn parse-renderings
  "Parse the rendering definitions based on defined objects."
  [renderings objects colors]
  (map #(assoc % :object ((:object %) objects) :color ((:color %) colors)) renderings))

(defn parse
  "Parse the given text into definitions and resolve references to previous objects."
  [text]
  (let [raw (edn/read-string text)
        objects (parse-objects (:definitions raw))
        colors (parse-colors (:colors raw))
        renderings (parse-renderings (:renderings raw) objects colors)]
    {:name (:name raw)
     :object objects
     :colors colors
     :renderings renderings}))

