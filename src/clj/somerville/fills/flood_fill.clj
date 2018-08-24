;; Provides a simple algorithm to flood fill an area
(ns somerville.fills.flood-fill
  (:require
    [clojure.set :as s]
    [somerville.geometry.point :as p]))

(defn neighbours
  "Filter list of points to those that are neighbours of the given point."
  [point]
  (list
    (p/point (- (p/x point) 1)  (- (p/y point) 1))
    (p/point (- (p/x point) 1)     (p/y point))
    (p/point (- (p/x point) 1)  (+ (p/y point) 1))
    (p/point    (p/x point)     (- (p/y point) 1))
    (p/point    (p/x point)     (+ (p/y point) 1))
    (p/point (+ (p/x point) 1)  (- (p/y point) 1))
    (p/point (+ (p/x point) 1)     (p/y point))
    (p/point (+ (p/x point) 1)  (+ (p/y point) 1))))

(defn in-bounds?
  "Check if a pixel is inside an image."
  [p x1 y1 x2 y2]
  (and
    (<  (p/x p) x2)
    (>= (p/x p) x1)
    (<  (p/y p) y2)
    (>= (p/y p) y1)))

(defn fill
  [seed previous decider-fn x1 y1 x2 y2]
  (loop [frontier #{seed}
         visited previous
         accepted #{}]
    (if (= 0 (count frontier))
      accepted
      (let [reachable (into #{} (filter #(decider-fn seed %) frontier))
            combined-frontier (into #{} (filter #(in-bounds? % x1 y1 x2 y2) (reduce concat (map #(neighbours %) reachable))))
            new-frontier (s/difference combined-frontier visited)]
        (recur new-frontier (s/union visited frontier) (s/union accepted reachable))))))

(defn partition-samples
  "Attempt to do sample-size flood fills from random points. If selected point was already visited the attempt fails."
  [sample-size decider-fn x1 y1 x2 y2]
  (filter
    #(< 50 (count (:points %)))
    (loop [i 0
           visited #{}
           partitions '()]
      (if (= i sample-size)
        partitions
        (let [seed (p/point (rand-int (+ x1 (- x2 x1))) (rand-int (+ y1 (- y2 y1))))
              part (if (nil? (some #{seed} visited))
                     (fill seed visited decider-fn x1 y1 x2 y2)
                     #{})]
          (recur (+ i 1) (s/union visited part) (conj partitions {:seed seed :points part})))))))

