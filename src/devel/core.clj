(ns devel.core
  (:require [devel.nrepl :as nrepl]))

(defn -main
  []
  (nrepl/start))
