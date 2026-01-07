(ns tieminos.lc.toplap-palestine-stream
  (:require
   [clojure.set :as set]
   [erv.beating-analyzer.v1 :refer [get-beat-data]]
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.seq-utils.core :refer [** ++ lin mirror rainseq]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2 :refer [on-event ref-rain]]))

;; Grady's metaslendro
(def metaslendro
  [49/48 25/24 7/6 19/16 4/3 65/48 3/2 37/24 151/96 7/4 43/24 2/1])

(defn +avg-freq
  [{:keys [root-hz
           ratio-1 ratio-1-partial
           ratio-2 ratio-2-partial]
    :as pair-data}]
  (assoc pair-data ::avg-freq
         (/ (+ (* root-hz ratio-1 ratio-1-partial)
               (* root-hz ratio-2 ratio-2-partial))
            2)))

(def beat-data
  (->> metaslendro
       (get-beat-data 2 1 [1 2 3 4 5 6 7 9 11 13])
       (map-indexed #(+avg-freq (assoc %2 ::id %1)))
       (filter #(> (::avg-freq %) 60))))

(def bf-map (group-by :beat-freq.ratio beat-data))

;; [bf-ratio bf-float]
(comment
  (map (juxt identity float) (sort (keys bf-map)))
  [[0N 0.0]
   [2/3 0.6666667]
   [1N 1.0]
   [4/3 1.3333334]
   [5/3 1.6666666]
   [2N 2.0]
   [7/3 2.3333333]
   [8/3 2.6666667]
   [3N 3.0]
   [10/3 3.3333333]
   [4N 4.0]
   [13/3 4.3333335]
   [14/3 4.6666665]
   [5N 5.0]
   [16/3 5.3333335]
   [17/3 5.6666665]
   [6N 6.0]
   [20/3 6.6666665]
   [7N 7.0]
   [22/3 7.3333335]
   [23/3 7.6666665]
   [8N 8.0]
   [26/3 8.666667]
   [28/3 9.333333]
   [10N 10.0]
   [32/3 10.666667]
   [34/3 11.333333]
   [35/3 11.666667]
   [12N 12.0]
   [37/3 12.333333]
   [38/3 12.666667]
   [13N 13.0]
   [40/3 13.333333]
   [41/3 13.666667]
   [14N 14.0]
   [44/3 14.666667]
   [15N 15.0]
   [46/3 15.333333]
   [47/3 15.666667]
   [16N 16.0]
   [49/3 16.333334]
   [50/3 16.666666]
   [17N 17.0]
   [52/3 17.333334]
   [53/3 17.666666]
   [18N 18.0]
   [56/3 18.666666]
   [58/3 19.333334]
   [59/3 19.666666]])

(oe/defsynth sini
  [freq 200
   freq-mul 1
   amp 0.5
   pan 0
   a 1
   s 1
   s-level 1
   r 1
   out 0]
  (o/out out
         (let [freq* (* freq freq-mul)]
           (-> freq*
               o/sin-osc
               (* amp (o/amp-comp freq*)
                  (lfo-kr 1 0 1)
                  (o/env-gen (o/envelope [0 1 s-level 0] [a s r])
                             :action o/FREE))
               (o/pan2 (* (lfo-kr 1 -0.5 0.5) pan))))))

(defn play-pair
  [params
   {:keys [root-hz
           ::avg-freq
           ratio-1 ratio-1-partial
           ratio-2 ratio-2-partial]}]

  (sini (merge {:freq (* root-hz ratio-1 ratio-1-partial)
                :pan -1
                :out 22}
               params))
  (sini (merge {:freq (* root-hz ratio-2 ratio-2-partial)
                :pan 1
                :out 22}
               params)))

;; original
(comment
  (rain.v2/stop)
  (ref-rain
   :id :pulse-rain
   :durs [1 3 2 5]
   :on-event
   (on-event
    (let [dur-amp (rainseq [3 3 3 5 3 5 3 5 5])]
      (play-pair {:a (* dur-amp  (rrange 0.1 2))
                  :s (* dur-amp (rrange 0.1 2))
                  :s-level (rrange 0.5 1)
                  :r (rrange 2 3)
                  :freq-mul (rainseq {1 10 2 3})
                  :amp 0.1}
                 (->> (bf-map (rainseq [(lin 2/3 2/3 10) 58/3 (lin 40/3 17) {5/3 10 16 3}]))
                      (wrap-at (rainseq [-1 1 -2 [3 -3] 8])))))))
  (ref-rain
   :id :pulse-rain2
   :durs [1 3 2 5]
   :ratio 1/8
   :on-event
   (on-event
    (let [dur-amp (* 0.1 (rainseq [3 3 3 5 3 5 3 5 5]))]
      (play-pair {:a (* dur-amp  (rrange 0.1 2))
                  :s (* dur-amp (rrange 0.1 2))
                  :s-level (rrange 0.5 1)
                  :r (rrange 2 3)
                  :freq-mul (rainseq {1 10 2 1})
                  :amp 0.1}
                 (->> (bf-map (rainseq [(lin 2/3 2/3 10) (lin 59/3 4/3) (lin 40/3 17) {5/3 10 16 3}]))
                      (wrap-at (rainseq [-1 1 -2 [3 -3] 8]))))))))

;; toplap livestream version
(comment
  (rain.v2/stop)
  (map (juxt identity float) (sort (keys bf-map)))
  (ref-rain
   :id :free-palestine
   :durs [1 3 2 5]
   :on-event
   (on-event
    (let [dur-amp (rainseq [3 3 3 5 3 5 3 5 5])]
      (play-pair {:a (* dur-amp  (rrange 0.1 2))
                  :s (* dur-amp (rrange 0.1 2))
                  :s-level (rrange 0.5 1)
                  :r (rrange 2 3)
                  :freq-mul (rainseq {1 10 2 3})
                  :amp 0.1}
                 (->> (bf-map (rainseq [(lin 1 13/3) 2/3 [14/3 5]]))
                      (wrap-at (rainseq [(lin 0 -2) 2 2 -3 -1])))))))
  (ref-rain
   :id :dont-stop-talking
   :durs [1 3 2 5]
   :ratio 1/11
   :on-event
   (on-event
    (when (> (rand) x)
      (let [dur-amp (* 0.1 (rainseq [3 3 3 5 3 5 3 5 5]))]
        (play-pair {:a (* dur-amp  (rrange 0.1 2))
                    :s (* dur-amp (rrange 0.1 2))
                    :s-level (rrange 0.5 1)
                    :r (rrange 2 3)
                    :freq-mul (rainseq {1 10 2 5 4 3})
                    :amp 0.1}
                   (->> (bf-map (rainseq [(lin 1 9 13/3) (lin 29/3 2/3) [14/3 5] 28/3 16]))
                        (wrap-at (rainseq [(lin 0 -2) 2 2 -3 -1]))))))))
  (def x 0.6)
  (ref-rain
   :id :for-a-world-that-can-fit-many-worlds
   :durs [1 3 2 5 1 1]
   :ratio 1/10
   :on-event
   (on-event
    (when (> (rand) x)
      (let [dur-amp (* 0.1 (rainseq [3 3 3 5 3 5 3 5 5]))]
        (play-pair {:a (* dur-amp  (rrange 0.1 2))
                    :s (* dur-amp (rrange 0.1 2))
                    :s-level (rrange 0.5 1)
                    :r (rrange 2 3)
                    :freq-mul (rainseq {1 10 2 5 4 3})
                    :amp 0.1}
                   (->> (bf-map (rainseq [(lin 58/3 1 5 13/3) (lin 29/3 2/3) (lin 14/3 5) 28/3 (lin 16 52/3)]))
                        (wrap-at (rainseq [(lin 0 -2) 1 2 -3 -1]))))))))
  (ref-rain
   :id :love-and-care-for-all
   :durs [7 3 2 5]
   :ratio 1/12
   :on-event
   (on-event
    (when (> (rand) x)
      (let [dur-amp (* 0.1 (rainseq [3 3 3 5 3 5 3 5 5]))]
        (play-pair {:a (* dur-amp  (rrange 0.1 2))
                    :s (* dur-amp (rrange 0.1 2))
                    :s-level (rrange 0.5 1)
                    :r (rrange 2 3)
                    :freq-mul (rainseq {1 10 2 5})
                    :amp 0.1}
                   (->> (bf-map (rainseq [(lin 58/3 1 5 13/3) (lin 29/3 2/3) (lin 14/3 5) 28/3 (lin 16 52/3)]))
                        (wrap-at (rainseq [(lin 0 -2) 1 2 -3 -1]))))))))
  (ref-rain
   :id :we-are-interdependent-we-are-palestine
   :durs [7 3 2 5]
   :ratio 1/5
   :on-event
   (on-event
    (let [dur-amp (* 0.1 (rainseq [3 3 3 5 3 5 3 5 5]))]
      (play-pair {:a (* dur-amp  (rrange 0.1 2))
                  :s (* dur-amp (rrange 0.1 2))
                  :s-level (rrange 0.5 1)
                  :r (rrange 2 3)
                  :freq-mul (rainseq {1 10})
                  :amp 0.1}
                 (->> (bf-map (rainseq [(lin 58/3 1 5 13/3) (lin 29/3 2/3) (lin 14/3 5) 28/3 (lin 16 52/3)]))
                      (wrap-at (rainseq [(lin 0 -2) 1 2 3 4 5]))))))))

(comment
  (->> bf-map
       (map (juxt first
                  (comp float first)
                  (comp count second)
                  (comp sort set #(mapcat (juxt :degree-1 :degree-2) %) second)))
       (sort-by second))

  (def bf-map2 (->> bf-map
                    (map (fn [[k data]]  [k (sort-by ::avg-freq data)]))
                    (into {})))

  (ref-rain
   :id :pulse-rain
   :durs [1 3 2 5 7]
   :on-event
   (on-event
    (when (> 0.1 (rand))
      (let [dur-amp (rainseq [3 3 3 5 3 5 3 5 5])]
        (play-pair {:a (* dur-amp  (rrange 0.1 2))
                    :s (* dur-amp (rrange 0.1 2))
                    :s-level (rrange 0.5 3)
                    :r (rrange 2 3)
                    :freq-mul (rainseq {1 10 2 6})
                    :amp (rainseq (** (range 0.05 0.1 0.01) [1  1 1 0.5 1]))}
                   (->> (bf-map2 (rainseq [(lin 58/3)

                                           (lin 2/3)]))
                        (wrap-at (rainseq [(lin -1 0 0)
                                           5
                                           (lin 2 0)]))))))))

  (def sorted-bd (sort-by ::avg-freq beat-data))

  (defn select-degrees
    [deg-set]
    (->> sorted-bd
         (filter (fn [{:keys [degree-1 degree-2]}]
                   (set/subset? #{degree-1 degree-2}
                                deg-set)))))

  (rain.v2/stop)
  (ref-rain
   :id :pulse-rain3
   :durs [1 3 2 5/2 1]
   :ratio 2/3
   :on-event
   (on-event
    (when (> 0.2 (rand))
      (let [dur-amp (rainseq (** 1/4 [3 3 3 5 3 5 3 5 5]))]
        (play-pair {:a (* dur-amp  (rrange 0.1 0.5))
                    :s (* dur-amp (rrange 0.1 2))
                    :s-level (rrange 0.5 2)
                    :r (rrange 2 3)
                    :freq-mul (rainseq {1 10 2 1 1/2 3 1/4 1})
                    :amp (rainseq (** [0.01 0.04] [1 2 1 1 0.5 1]))}
                   (->> (select-degrees (rainseq (repcat [10 #{0 3 5 7}]
                                                         [10 #{1 3 5 7}]
                                                         [10 #{1 3 5 8}]
                                                         [10 #{1 3 11 8}]
                                                         [10 #{1 4 11 8}])))
                        (wrap-at (rainseq (++ (repcat [20 -10]
                                                      [20 [-30]]
                                                      [10 [-40]])
                                              (reverse (range 40))))))))))))
