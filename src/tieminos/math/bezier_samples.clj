(ns tieminos.math.bezier-samples
  "Some sample curves"
  (:require [tieminos.math.bezier :as bz]
            [tieminos.math.utils :refer [linexp linlin]]))

;; f = fast
;; s = slow

(defn fsf
  ([steps] (fsf steps 0.01 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur
           (bz/curve steps
                     [15 1 0.1 (rand-nth [26 20]) 7 0.1]))))

(defn f
  ([steps] (f steps  0.03 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur (bz/curve steps [(rand-nth [20 12 5]) 0.1 0.1 6 1]))))

(defn s
  ([steps] (s steps  0.03 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur (bz/curve steps [0.1 (rand-nth [1 5 10])]))))

(comment
  (bz/plot (fsf 20)))
