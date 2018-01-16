(ns devel.core
  (:require
    [devel.nrepl :as nrepl]
    [devel.tests :as tests]
    [reply.main :as reply]))

(defn start-nrepl
  []
  (nrepl/start))

(defn stop-nrepl
  []
  (nrepl/stop))

(defn run-tests
  []
  (tests/run))

(defn -main
  []
  ;(reply/launch-standalone {}))
  (clojure.main/repl))

