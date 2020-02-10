(ns somerville.visualization.worker
  (:require
    [somerville.visualization.commons :as commons]
    [somerville.maps.gaia.core :as gaia]
    [taoensso.timbre :as log]))


;;====================================================================================================
;; Work handling for gaia

(defn cube
  "Make a cube."
  []
  (gaia/cube 150))

(defn icosahedron
  "Make an icosahedron."
  []
  (gaia/icosahedron 400))

(defn fibonacci
  "Make a fibonacci point sphere."
  []
  (gaia/fibonacci 200))

(defn delaunay
  "Make a delaunay sphere."
  []
  (gaia/delaunay 200))

(defn voronoi
  "Make a voronoi sphere."
  []
  (gaia/voronoi 100))

(defn subdivide
  "Subdivide current world"
  [world]
  (gaia/subdivide world))

(defn dispatch-gaia
  "Handle work requests for gaia."
  [message]
  (case (:command message)
    "fibonacci"    (fibonacci)
    "delaunay"     (delaunay)
    "voronoi"      (voronoi)
    "icosahedron"  (icosahedron)
    "cube"         (cube)
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

