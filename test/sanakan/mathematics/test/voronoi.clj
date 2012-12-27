(ns sanakan.mathematics.test.voronoi
  (:use [sanakan.mathematics.voronoi])
  (:use [clojure.test]))

(deftest flat
         (let [t1 {:left nil :right nil :site 1}
               t2 {:left nil :right nil :site 2}
               t3 {:left t1 :right t2 :site 3}
               t4 {:left t3 :right t2 :site 4}
               s1 (sites t3)
               s2 (sites t4)]
           (is (= (count s1) 2))
           (is (= (nth s1 0) 1))
           (is (= (nth s1 1) 2))
           (is (= (count s2) 3))
           (is (= (nth s2 0) 1))
           (is (= (nth s2 1) 2))
           (is (= (nth s2 2) 2))
           ))
