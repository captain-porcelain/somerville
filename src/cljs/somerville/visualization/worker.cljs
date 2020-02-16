(ns somerville.visualization.worker
  (:require
    [somerville.visualization.commons :as commons]
    [somerville.maps.gaia.core :as gaia]
    [taoensso.timbre :as log]))


;;====================================================================================================
;; Work handling for gaia

(defn merge-config
  "Create the config for gaia."
  [data]
  (merge gaia/default-config data))

(defn fibonacci
  "Make a fibonacci point sphere."
  [data]
  (gaia/fibonacci (merge-config data)))

(defn delaunay
  "Make a delaunay sphere."
  [data]
  (gaia/delaunay (merge-config data)))

(defn voronoi
  "Make a voronoi sphere."
  [data]
  (gaia/voronoi (merge-config data)))

(defn dispatch-gaia
  "Handle work requests for gaia."
  [message]
  (case (:command message)
    "fibonacci"    (fibonacci (:data message))
    "delaunay"     (delaunay (:data message))
    "voronoi"      (voronoi (:data message))))


;;====================================================================================================
;; Dispatch handling

(defn dispatch-all
  "Dispatch all incoming messages by requested module."
  [message]
  (try
    (case (:module message)
      "gaia" (dispatch-gaia message)
      (log/info "Don't know module " (:module message)))
    (catch js/Object e "Error in worker")))

(defn handle-message
  [msg]
  (let [message (commons/to-clojurescript (.-data msg))
        tmp (log/info "Received work message " (:module message) " -> " (:command message))
        result (dispatch-all message)]
    (.postMessage js/self (commons/to-arraybuffer (assoc message :result result)))))

(defn start
  "Start the worker message processing."
  []
  (set! (.-onmessage js/self) handle-message)
  (.postMessage js/self (commons/to-arraybuffer {:command "worker-start"})))

(start)

