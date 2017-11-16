(ns somerville.commons
  (:import
    [java.io InputStreamReader BufferedReader]))

(defn in?
  "Check that itm is not in itms."
  [itm itms]
  (false? (nil? (some #(= itm %) itms))))

(defn get-random
  "Randomly select one of elements."
  [elements]
  (let [c (count elements)]
    (if (= 0 c)
      nil
      (nth elements (rand-int c)))))

(defn resource-stream
  "Get an InputStream from a resource"
  [resource-name]
  (let [thr (Thread/currentThread)
        ldr (if (nil? thr) nil (.getContextClassLoader thr))]
    (if (nil? ldr) nil (.getResourceAsStream ldr resource-name))))

(defn list-resources
  "List resources available at path."
  [path]
  (line-seq (BufferedReader. (InputStreamReader. (resource-stream path)))))
