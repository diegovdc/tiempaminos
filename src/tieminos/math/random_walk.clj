(ns tieminos.math.random-walk)

(defn- rand-range [a b]
  (+ a (rand (- b a))))

(defn rand-walk1
  [max-step-size length]
  (let [steps (repeatedly #(rand-range (* -1 max-step-size) max-step-size))]
    (reductions + (take length steps))))
