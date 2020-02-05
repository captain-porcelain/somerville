(ns somerville.server
  (:require
    [compojure.core :as compojure]
    [compojure.handler :as ch]
    [ring.util.response :as rup]
    [ring.util.servlet :as rus]
    [org.httpkit.server :as httpkit]
    [taoensso.timbre :as log]))

(def instance (atom nil))

(compojure/defroutes
  handler

  (compojure/GET "/:f.:e" [f e]
    (rup/resource-response (str "html/" f "." e)))
  (compojure/GET "/img/:f.:e" [f e]
    (rup/resource-response (str "images/" f "." e)))
  (compojure/GET "/js/:f.:e" [f e]
    (rup/resource-response (str "js/" f "." e))))

(defn wrap-request-logging
  "Log each request with extra information."
  [handler]
  (fn [{:keys [request-method uri] :as req}]
    (let [start  (System/currentTimeMillis)
          resp   (handler req)
          finish (System/currentTimeMillis)
          total  (- finish start)]
      (log/info (str "TIMING: " request-method " to " uri " took " total "ms."))
      resp)))

(def app
  (-> #'handler
      (wrap-request-logging)))

(defn start
  "Start the http server."
  []
  (rus/defservice app)
  (reset! instance (httpkit/run-server (ch/site app) {:port 9192})))

(defn stop
  "Stop the http server."
  []
  (@instance))

