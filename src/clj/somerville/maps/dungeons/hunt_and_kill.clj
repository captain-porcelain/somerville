(ns somerville.maps.dungeons.hunt-and-kill
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.maps.grid :as grid]))

(defn hunt-candidate
  "Get an unvisited cell that neighbors to a visited one."
  [g]
  (last
    (filter #(and (< 0 (count (grid/visited-neighbors g %)))
                   (= 0 (count (:links %))))
            (grid/unmasked-cells g))))

(defn hunt-and-kill-iterator
  "Run Hunt-and-Kill Algorithm on a grid."
  [g c remaining]
  (let [candidates (grid/unvisited-neighbors g c)]
    (if (= 0 (count candidates))
      (let [c (hunt-candidate g)
            c2 (if (nil? c) (grid/get-from g (first remaining)) c)
            tmp (when-not (nil? c) (grid/place-link g c (:direction (commons/get-random (grid/visited-neighbors g c)))))]
        [(p/x c2) (p/y c2)])
      (let [c2 (commons/get-random candidates)
            next-cell [(p/x (:cell c2)) (p/y (:cell c2))]
            tmp (when (commons/in? next-cell remaining) (grid/place-link g c (:direction c2)))]
        next-cell))))

(defn maze
  "Create a maze using Hunt-and-Kill algorithm."
  ([g]
   (grid/iterate-all g (commons/get-random (grid/unmasked-cell-coordinates g)) hunt-and-kill-iterator))
  ([width height]
   (maze (grid/grid width height)))
  ([width height mask-text]
   (maze (grid/walk (grid/grid width height) (grid/make-mask-walker mask-text)))))

