(ns somerville.dungeons.generators.sidewinder
  (:require
    [somerville.commons :as commons]
    [somerville.dungeons.generators.grid :as grid]))


(defn close?
  "Check if sidewinder should close the current run."
  [directions]
  (or
    (not (commons/in? :east directions))
    (and
      (commons/in? :south directions)
      (= 0 (rand-int 2)))))

(defn sidewinder-line-walker
  "Handle one grid row with sidewinder logic."
  [g cells]
  (loop [run (list)
         remaining cells]
    (if (= 0 (count remaining))
      run
      (let [directions (map :direction (grid/neighbor-cells g (first remaining)))
            close (close? directions)
            current-run (conj run (first remaining))
            c (if close (commons/get-random current-run) (first remaining))
            link (if close
                   (when (commons/in? :south directions) (grid/place-link g c :south))
                   (when (commons/in? :east  directions) (grid/place-link g c :east)))
            new-run (if close (list) current-run) ]
        (recur new-run (rest remaining))))))

(defn maze
  "Create a maze using sidewinder algorithm."
  ([g]
   (grid/walk-lines g sidewinder-line-walker))
  ([width height]
   (maze (grid/grid width height)))
  ([width height mask-text]
   (maze (grid/walk (grid/grid width height) (grid/make-mask-walker mask-text)))))

