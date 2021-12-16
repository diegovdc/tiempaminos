(ns erv-fib-synth.math.bezier
  "Bezier curves.
  Following: https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Explicit_definition
  and https://040code.github.io/2017/07/01/visualising-bezier-curves"
  (:require
   [erv-fib-synth.math.utils :refer [linexp linlin]]
   [erv.math.pascals-triangle :as pascal]
   [helins.interval.map :as imap]
   [incanter.charts :as charts]
   [incanter.core :as incanter]))

(defn curve*
  [timesteps points]
  (let [row (pascal/row (dec (count points)))
        curve-fns (->> row
                       (map-indexed
                        (fn [i coeff]
                          (fn [t]
                            (*' coeff
                                (Math/pow t i)
                                (Math/pow (- 1 t)
                                          (- (count points) (inc i)))
                                (nth points i))))))]
    (with-meta (map (fn [t] (apply + (map #(% t) curve-fns)))
                    timesteps) {:points points})))


(defn curve [num-timesteps points]
  (curve* (range 0 1 (/ 1 num-timesteps)) points))

(comment
  (defn pow [base exponent]
    (reduce *' (repeat exponent base))))


(defn plot-xy
  ([xs ys] (plot-xy xs ys ""))
  ([xs ys plot-title]
   (let [dataset (incanter/conj-cols xs ys)
         xy-plot (charts/xy-plot 0 1 :data dataset :points true :title plot-title)]
     (incanter/view xy-plot))))

(defn plot
  ([curve] (plot curve (-> curve meta :points str (or "anonymous plot"))))
  ([curve plot-title]
   (let [xs (range (count curve))
         ys curve
         dataset (incanter/conj-cols xs ys)
         xy-plot (charts/xy-plot 0 1 :data dataset :points true :title plot-title)]
     (incanter/view xy-plot))))


(comment
  (plot-xy (curve* (range 0 1 (/ 1 100)) [10 20 90 220])
           (curve* (range 0 1 (/ 1 100)) [0 25 0 2]) "test")
  (plot (curve (range 0 1 (/ 1 100)) [0 25 0 2]) "test"))

(def xys (map vector
              (curve* (range 0 1.2 (/ 1 5)) (map #(/ % 5) [1 2 3 4 5]))
              (curve* (range 0 1.2 (/ 1 5)) [1 2 3 4 5])))


(defn to-intervals
  ([ys] (to-intervals (range 1 (inc (count ys))) ys))
  ([xs ys]
   (:imap (reduce (fn [{:keys [imap prev-x]} [x y]]
                    {:imap (imap/mark imap prev-x x y)
                     :prev-x x})
                  {:imap imap/empty
                   :prev-x 0}
                  (map vector xs ys)))))

(comment
  ;; interval-usage
  (def music
    (-> imap/empty
        ;; Broken C minor chord.
        (imap/mark  0  8 :c)
        (imap/mark  3  8 :e-flat)
        (imap/mark  5  8 :g)
        ;; After a pause, G is repeated.
        (imap/mark 10 11 :g)))
  (music 5)


  (def c (->> (curve 10 [10 2 20 1.2 1 1 1])
              (linexp 1 5)
              to-intervals))
  (c 4))
