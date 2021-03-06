(ns tieminos.math.utils)

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

(linexp 1 5 [1 2 3 4 5])

(= (map float (linexp 1 3 [1 2 3 4 5]))
   (map float [1.0 1.3160740129525 1.7320508075689 2.2795070569548 3.0]))
