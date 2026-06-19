(ns tieminos.math.bezier
  "Bezier curves.
  Following: https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Explicit_definition
  and https://040code.github.io/2017/07/01/visualising-bezier-curves"
  (:require
   [erv.math.pascals-triangle :as pascal]
   [helins.interval.map :as imap]
   [incanter.charts :as charts]
   [incanter.core :as incanter]
   [tieminos.math.utils :refer [linexp linlin]]))

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
    (with-meta (mapv (fn [t] (apply + (map #(% t) curve-fns)))
                     timesteps) {:points points})))

(defn curve [num-timesteps points]
  (curve* (range 0 1 (/ 1 num-timesteps)) points))

(comment
  (defn pow [base exponent]
    (reduce *' (repeat exponent base))))

(defonce plots (atom {}))

;; TODO: figure out what was this for
(defn plot-xy
  ([xs ys] (plot-xy :default xs ys ""))
  ([id xs ys plot-title]
   (let [dataset (incanter/conj-cols xs ys)
         xy-plot (charts/xy-plot 0 1 :data dataset :points true :title plot-title)
         view (incanter/view xy-plot)
         prev-view (get @plots id)]
     (when prev-view (.dispose prev-view))
     (swap! plots assoc id view)
     view)))

(defn plot
  ([curve] (plot :default curve (-> curve meta :points str (or "anonymous plot"))))
  ([id curve plot-title]
   (let [xs (range (count curve))
         ys curve
         dataset (incanter/conj-cols xs ys)
         xy-plot (charts/xy-plot 0 1 :data dataset :points true :title plot-title)
         view (incanter/view xy-plot)
         prev-view (get @plots id)]
     (when prev-view (.dispose prev-view))
     (swap! plots assoc id view)
     view)))

(comment
  (plot-xy (curve* (range 0 1 (/ 1 100)) [10 20 90 220])
           (curve* (range 0 1 (/ 1 100)) [0 25 0 2]) "test")
  (plot (map #(max 1.3 (min % 4)) (curve 200 (concat [1.3] (flatten (shuffle [[3.3 2.3] [3.5 -1.5 2.5 4.5] [6.2 7.7 -1.4] [-3 5.5 5 2 3.5] 2]))))) "test"))

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
