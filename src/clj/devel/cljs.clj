(ns devel.cljs
  (:require
    [devel.config :as config]
    [cljs.build.api :as api]))

(defn compile-once
  []
  (api/build (:cljs-sources @config/config) (:cljs-options @config/config)))

(defn watch
  []
  (api/watch (:cljs-sources @config/config) (:cljs-options @config/config)))


;(defmethod task "figwheel" [[_ port]]
  ;(with-namespaces [figwheel-sidecar.repl-api]
    ;(figwheel-sidecar.repl-api/start-figwheel!
     ;{:figwheel-options (cond-> {}
                          ;port (merge {:nrepl-port       (some-> port Long/parseLong)
                                       ;:nrepl-middleware ["cider.nrepl/cider-middleware"
                                                          ;"refactor-nrepl.middleware/wrap-refactor"
                                                          ;"cemerick.piggieback/wrap-cljs-repl"]}))
      ;:all-builds       [{:id           "dev"
                          ;:figwheel     true
                          ;:source-paths [source-dir]
                          ;:compiler     dev-config}]})
    ;(when-not port
      ;(figwheel-sidecar.repl-api/cljs-repl))))


