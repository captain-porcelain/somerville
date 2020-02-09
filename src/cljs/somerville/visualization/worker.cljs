(ns somerville.visualization.worker
  (:require
    [taoensso.timbre :as log]))

(defn do-work
  []
  (.log js/console "Worker started"))

(set! (.-onmessage js/self) do-work)
