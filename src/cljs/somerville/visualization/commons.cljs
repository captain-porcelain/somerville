(ns ^:figwheel-hooks somerville.visualization.commons
  (:require
    [taoensso.timbre :as log]))

(defonce text-encoder (js/TextEncoder.))
(defonce text-decoder (js/TextDecoder.))

(defn to-arraybuffer
  "Convert cljs object to arraybuffer."
  [o]
  (.encode text-encoder (.stringify js/JSON (clj->js o))))

(defn to-clojurescript
  "Convert arraybuffer to cljs object."
  [a]
  (js->clj (.parse js/JSON (.decode text-decoder a)) :keywordize-keys true))

