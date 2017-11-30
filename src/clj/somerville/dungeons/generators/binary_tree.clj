(ns somerville.dungeons.generators.binary-tree
  (:require
    [somerville.commons :as commons]
    [somerville.dungeons.generators.grid :as grid]))


(defn binary-tree-walker
  "Execute binary tree decision to link either south or east for each grid cell."
  [g e]
  (grid/place-link g e (commons/get-random (remove #{:west :north} (map :direction (grid/neighbor-cells g e))))))

(defn maze
  "Create a maze using binary tree algorithm."
  ([g]
   (grid/walk g binary-tree-walker))
  ([width height]
   (maze (grid/grid width height)))
  ([width height mask-text]
   (maze (grid/walk (grid/grid width height) (grid/make-mask-walker mask-text)))))

