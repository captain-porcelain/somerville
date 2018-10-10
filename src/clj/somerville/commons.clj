(ns somerville.commons
  (:import
    [java.io InputStreamReader BufferedReader]))

(defn abs
  "Calculate absolute value."
  [n]
  (if (< n 0) (* -1 n) n))

(defn in?
  "Check that itm is not in itms."
  [itm itms]
  (false? (nil? (some #(= itm %) itms))))

(defn get-random
  "Randomly select one of elements."
  [elements]
  (let [c (count elements)]
    (if (= 0 c)
      nil
      (nth elements (rand-int c)))))

(defn resource-stream
  "Get an InputStream from a resource"
  [resource-name]
  (let [thr (Thread/currentThread)
        ldr (if (nil? thr) nil (.getContextClassLoader thr))]
    (if (nil? ldr) nil (.getResourceAsStream ldr resource-name))))

(defn list-resources
  "List resources available at path."
  [path]
  (line-seq (BufferedReader. (InputStreamReader. (resource-stream path)))))

(defn parse-int
  "Try to parse a string as a integer."
  ([^String n default]
   (try
     (Integer/parseInt n)
     (catch Exception e default)))
  ([^String n]
   (parse-int n 0)))

(defn d
  [sides]
  (+ (rand-int sides) 1))

(def d4 #(d 4))
(def d6 #(d 6))
(def d8 #(d 8))
(def d10 #(d 10))
(def d12 #(d 12))
(def d20 #(d 20))
(def d100 #(+ (d 10) (* 10 (- (d 10) 1))))

(defn roll
  [amount die]
  (take amount (repeatedly die)))

(defn attribute
  []
  (reduce + (rest (sort (roll 4 d6)))))

(defn attributes
  []
  (let [atts (rest (sort (take 7 (repeatedly attribute))))]
    (str (clojure.string/join ", " atts) " -> " (reduce + atts))))

;; Dates when cities were founded according to Friedenschor calendar
(def aersiire {:year -6765 :phase 8 :day 26}); 26.08.6765
(def anan-elunore {:year -6527 :phase 7 :day 28}) ; 28.07.6527
(def estellad {:year -6323 :phase 7 :day 4}) ; 04.07.6323

;; Length of the 'weeks'
(def aerdeth {:length 76})
(def sariel {:length 23})
(def palias {:length 7})

(def first-reunion {:year -16520 :phase 11 :day 4})

(defn days-to-first
  "Calculate amount of days till next years 1.1."
  [p d]
  (+ 1 (+ (- 30 d) (* 30 (- 12 p)))))

(defn days-since-first
  "Calculate amount of days since this years 1.1."
  [p d]
  (- (+ d (* 30 (- p 1))) 1))

(defn days-between
  "Calculate amount of days between two dates."
  [d1 d2]
  (let [t (days-to-first (:phase d1) (:day d1))
        f (days-since-first (:phase d2) (:day d2))
        ys (- (:year d2) (+ (:year d1) 1))]
    (+ (* 360 ys) t f)))

(defn add-days
  [d days]
  (let [ntd (+ (:day d) days)
        nd (mod ntd 30)
        pco (int (/ ntd 30))
        ntp (+ (:phase d) pco)
        np (mod ntp 12)
        nyc (int (/ ntp 12))
        ny (+ (:year d) nyc)]
    {:year ny :phase np :day nd}))

(defn moon-date
  [d cal start]
  (let [ds (days-between start d)
        unions (int (/ ds 12236))
        rem1 (mod ds 12236)
        weeks (int (/ rem1 (:length cal)))
        days (mod rem1 (:length cal))]
    {:union unions :week weeks :day days}))

(def reunions (map #(add-days first-reunion (* 12236 %)) (iterate inc 0)))

