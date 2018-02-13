(ns somerville.maps.mazes.aldous-broder
  (:require
    [somerville.commons :as commons]
    [somerville.maps.grid :as grid]))


(defn aldous-broder-iterator
  "Run Aldous-Broder Algorithm on a grid."
  [g c remaining]
  (let [candidate (commons/get-random (grid/neighbor-cells g c))
        next-cell [(:x (:cell candidate)) (:y (:cell candidate))]
        tmp (when (commons/in? next-cell remaining) (grid/place-link g c (:direction candidate)))]
    next-cell))

(defn maze
  "Create a maze using Aldous-Broder algorithm."
  ([g]
   (grid/iterate-all g (commons/get-random (grid/unmasked-cell-coordinates g)) aldous-broder-iterator))
  ([width height]
   (maze (grid/grid width height)))
  ([width height mask-text]
   (maze (grid/walk (grid/grid width height) (grid/make-mask-walker mask-text)))))

