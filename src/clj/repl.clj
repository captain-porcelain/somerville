(ns repl
  (:require
    [nrepl.server :as nrepl-server]
    [cider.nrepl :refer [cider-nrepl-handler]]
    [rebel-readline.main :as rebel]))

(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defn -main []
  (nrepl-server/start-server :port 7888 :handler (nrepl-handler))
  (rebel/-main)
  (System/exit 0))
