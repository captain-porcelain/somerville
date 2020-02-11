(ns somerville.visualization.worker
  (:require
    [somerville.visualization.commons :as commons]
    [somerville.maps.gaia.core :as gaia]
    [taoensso.timbre :as log]))


;;====================================================================================================
;; Work handling for gaia

(defn cube
  "Make a cube."
  [data]
  (gaia/cube (:scale data)))

(defn icosahedron
  "Make an icosahedron."
  [data]
  (gaia/icosahedron (:scale data)))

(defn fibonacci
  "Make a fibonacci point sphere."
  [data]
  (gaia/fibonacci (:fibonacci-size data) (:scale data)))

(defn delaunay
  "Make a delaunay sphere."
  [data]
  (gaia/delaunay (:fibonacci-size data) (:scale data)))

(defn voronoi
  "Make a voronoi sphere."
  [data]
  (gaia/voronoi (:fibonacci-size data) (:scale data)))

(defn subdivide
  "Subdivide current world"
  [world]
  (gaia/subdivide world))

(defn dispatch-gaia
  "Handle work requests for gaia."
  [message]
  (case (:command message)
    "fibonacci"    (fibonacci (:data message))
    "delaunay"     (delaunay (:data message))
    "voronoi"      (voronoi (:data message))
    "icosahedron"  (icosahedron (:data message))
    "cube"         (cube (:data message))
    "subdivide"    (subdivide (:world (:data message)))))


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

