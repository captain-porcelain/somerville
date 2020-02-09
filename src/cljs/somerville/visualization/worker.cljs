(ns somerville.visualization.worker
  (:require
    [taoensso.timbre :as log]))

(defn do-work
  [msg]
  (log/info (str "Received message " (.stringify js/JSON (.-data msg)))))

(set! (.-onmessage js/self) do-work)
