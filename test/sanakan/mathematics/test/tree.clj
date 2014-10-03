(ns sanakan.mathematics.test.tree
  (:use [sanakan.mathematics.geometry.geometry])
  (:use [clojure.test]))

(defn compare-key-fn [key1 key2] (< (:y key1) (:y key2)))

(deftest test-tree
         (let [t (->                              ;;syntax sugar
                   (sorted-set-by compare-key-fn) ;;this returns empty tree with given function to compare keys
                   (conj (struct-map point2 :x 2 :y 5))
                   (conj (struct-map point2 :x 1 :y 2))
                   (conj (struct-map point2 :x 4 :y 9))
                   (conj (struct-map point2 :x 3 :y 7)))]
;           (prn t)
           (is (= 1 (:x (first t))))
           (is (= 2 (:x (first (rest t)))))
           (is (= 3 (:x (first (rest (rest t))))))
           (is (= 4 (:x (first (rest (rest (rest t)))))))))

(def sites (list
             (struct-map point2 :x 2 :y 5)
             (struct-map point2 :x 4 :y 9)
             (struct-map point2 :x 1 :y 2)
             (struct-map point2 :x 3 :y 7)
             ))

(deftest test-tree2
         (let [t (reduce conj (sorted-set-by compare-key-fn) sites)]
;           (prn t)
           (is (= 1 (:x (first t))))
           (is (= 2 (:x (first (rest t)))))
           (is (= 3 (:x (first (rest (rest t))))))
           (is (= 4 (:x (first (rest (rest (rest t)))))))))

