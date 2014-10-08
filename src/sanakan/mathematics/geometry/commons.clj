(ns sanakan.mathematics.geometry.commons)

(defn indent
  [i]
  (reduce str (repeat i " ")))

;; Define a protocol for pretty printing geometry records.
(defprotocol Printable
  (out [this] [this x]))

