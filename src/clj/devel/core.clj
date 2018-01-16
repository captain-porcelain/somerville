(ns devel.core
  (:require
    [clojure.edn :as edn]
    [devel.config :as config]
    [devel.nrepl :as nrepl]
    [devel.cljs :as cljs]
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

(defn watch-tests
  []
  (tests/watch))

(defn compile-cljs
  []
  (cljs/compile-once))

(defn watch-cljs
  []
  (cljs/watch))

(defn stop-watching-tests
  []
  (tests/stop-watching))

(defn load-config
  []
  (reset! config/config (edn/read-string (slurp "devel.edn"))))

(defn -main
  []
  (load-config)
  (reply/launch-standalone {:color true})
  (System/exit 0))
