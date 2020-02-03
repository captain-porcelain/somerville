(ns somerville.resources
  (:import
    [java.io InputStreamReader BufferedReader]))

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

