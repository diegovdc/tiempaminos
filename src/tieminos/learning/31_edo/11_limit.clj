(ns tieminos.learning.31-edo.11-limit
  (:require
   [erv.edo.core :as edo]
   [erv.utils.core :refer [period-reduce]]
   ;; FIXME uncomment when I have the latest version
   ;; [erv.utils.ratios :refer [ratio-proximity-list]]
   [erv.utils.conversions :as conv]))

(def *31edo
  (:scale (edo/from-pattern (repeat 31 1))))

(def over-11
  "33 unique functions"
  (let [limit-of-11 5]
    (->> (range 1 (inc (* 11 limit-of-11)))
         (mapcat (fn [n] (map #(period-reduce (/ n (* 11 %))) (range 1 (inc limit-of-11)))))
         set
         sort)))

(comment

  (->> (ratio-proximity-list (map :bounded-ratio *31edo)
                             over-11
                             10)
       (map (fn [[deg ratios]]
              [deg (map (juxt :ratio :diff) ratios)])))

  (ratio-proximity-list (map :bounded-ratio *31edo)
                        [41/22]
                        15)

  (map :bounded-ratio *31edo)
  (->> *31edo
       (map (juxt :edo/degree :bounded-ratio)))

  (-> over-11 sort)
  (float 729/512)
  (float 15/12)
  (- (Conv/ratio->cents 1.3675683155263922)
     (conv/ratio->cents (float 11/8)))
  (- (conv/ratio->cents 1.3675683155263922)
     (conv/ratio->cents (* 1.022 (float 11/9)))))
