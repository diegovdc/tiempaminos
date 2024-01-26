(ns tieminos.math.utils
  (:require
   [erv.utils.core :refer [round2]]))

(defn normalize [ns]
  (let [sum (apply + ns)]
    (map #(/ % sum) ns)))

;; TODO implement for non lists
;; mapping functions
;; https://github.com/supercollider/supercollider/blob/18c4aad363c49f29e866f884f5ac5bd35969d828/lang/LangSource/MiscInlineMath.h

(defn expexp [in-min in-max out-min out-max nums]
  (throw "Implement me"))

(defn explin [in-min in-max out-min out-max nums]
  (throw "Implement me"))

(do
  (defn linlin
    "Linear to linear scaling"
    ([out-min out-max nums]
     (linlin (apply min nums) (apply max nums) out-min out-max nums))

    ([in-min in-max out-min out-max nums]
     (let [in-range (- in-max in-min)
           out-range (- out-max out-min)]
       (map #(-> % (* out-range) (+ out-min))
            (map #(/ (- % in-min) in-range) nums)))))

  (= (mapv float (linlin 1 5 1 3 [1 2 3 4 5]))
     [1.0, 1.5, 2.0, 2.5, 3.0]))

(comment
  (mapv float (linlin 5 0.5 1 3 [1 2 3 4 5])))

(defn linexp*
  "Linear to exponential scaling"
  [in-min in-max out-min out-max x]
  (* out-min (Math/pow (/ out-max out-min)
                       (/ (- x in-min) (- in-max in-min)))))

(defn linexp
  ([out-min out-max nums]
   (linexp (apply min nums) (apply max nums) out-min out-max nums))

  ([in-min in-max out-min out-max nums]
   (map (partial linexp* in-min in-max out-min out-max) nums)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn logscale
  ;; Based on https://stackoverflow.com/a/28132981, only works when min is 0, but there are other options in this answer
  "Logarithmically scale `x` with respect to `max`.
  Only works when the minimum value is 0.
  Not entirely sure what I am doing, but seems to work if `x` is not > `max`...
  NOTE a log<X>(x) is defined as ln(x)/ln(X), to aviod -Infinity as a result +1 is added to x"
  [max x]
  (/ (Math/log (+ 1 x))
     (Math/log (+ 1 max))))

(defn avg
  [xs]
  (if-not (seq xs)
    0
    (/ (apply + xs) (count xs))))

(defn linearly-weighted-avg
  [xs]
  (if-not (seq xs)
    0
    (let [total (count xs)]
      (/ (apply + (map-indexed
                   (fn [i x] (* x (- total i)))
                   xs))
         (apply + (range (inc total)))))))

(defn exponentially-weighted-avg
  [alpha xs]
  (if-not (seq xs)
    0
    (let [n (count xs)]
      (/ (apply + (map-indexed
                   (fn [i x] (* x (Math/pow alpha (- n i))))
                   xs))
         (apply + (map (fn [i] (Math/pow alpha (- n i))) (range 1 (inc n))))))))


;; TODO add tests
(linearly-weighted-avg [1 1 1 1])
(linearly-weighted-avg [1 9/10 8/10 7/10 6/10 5/10])
(linearly-weighted-avg (reverse [1 9/10 8/10 7/10 6/10 5/10]))
(linearly-weighted-avg (range 1 -1/10 -1/10))
(linearly-weighted-avg (reverse (range 1 -1/10 -1/10)))

(defn hyperbolic-tangent
  ([x] (hyperbolic-tangent 1 x))
  ([scale x]
   (/ (- (Math/pow Math/E (* scale x)) 1)
      (+ (Math/pow Math/E (* scale x)) 1))))

(defn hyperbolic-decay
  ;; NOTE initially intended as a feedback mechanism for controlling amplitudes
  "Works with values between 0 and 1.
  `max-threshold` is the value after which the hyperbolic function is applied
  `scale` is the steepness of the curve.

  NOTE: This is almost surely not mathematically sound and may be better solution for which I am trying to do here."
  [x scale max-threshold]
  (- 1 (max 0 (hyperbolic-tangent (- x max-threshold)
                                  scale))))
