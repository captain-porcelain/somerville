(ns sanakan.mathematics.geometry.commons)

;; Define a protocol for pretty printing geometry records.
(defprotocol Printable
  (out [x]))

