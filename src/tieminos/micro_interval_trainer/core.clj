(ns tieminos.micro-interval-trainer.core
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.utils.conversions :as convo]
   [erv.utils.core :refer [interval period-reduce]]
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.synths :refer [low short-plate]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(oe/defsynth it-synth
  [freq 200
   amp 0.5
   atk 1
   dcy 1]
  (o/out 0 (-> (o/saw freq)
               (o/lpf 3500)
               (o/pan2 (o/lf-noise1 0.3))
               (* amp (o/env-gen (o/env-perc atk dcy))))))

(defn chord
  [& {:keys [base-freq intervals dur synth]
      :or {synth low}}]
  (ref-rain
   :id ::chord
   :loop? false
   :durs [dur]
   :on-event (on-event
              (doseq [interval intervals]
                (synth :freq (* #_(rand-nth [1 2 #_3 4 1/2 #_1/3 1/4]) base-freq interval)
                       :atk (/ dur 2)
                       :mod-freq (rrand 300 8000)
                       :dcy (/ dur 2))))))

(defn melody
  [& {:keys [base-freq intervals dur synth]
      :or {synth low}}]
  (ref-rain
   :id ::meody
   :loop? false
   :durs (repeat (count intervals) (/ dur (count intervals)))
   :on-event (on-event
              (synth
               :freq (* base-freq (nth intervals index))
               :mod-freq (rrand 300 8000)
               :atk (/ dur 3)
               :dcy (/ dur 2/3)))))

(comment
  ;; menu of some rational intervals
  (let [numerators (range 1 26 2)
        denominators (range 1 14 2)]
    (->> numerators
         (mapcat (fn [n] (map (fn [d] (period-reduce 2 (/ n d))) denominators)))
         set
         sort
         #_(filter (fn [r] (and (ratio? r) ((set (prime-factors (denominator r))) 7))))
         (map (fn [r]
                {:ratio r
                 :cents (convo/ratio->cents r)}))))
  (convo/ratio->cents 8/7)
  (map convo/ratio->cents (edo/edo-ratios 31)))

(defn get-cps-diads [cps-scale]
  (->> (combo/combinations cps-scale 2)
       (filter (fn [[a b]]
                 (seq (set/intersection (:archi-set a) (:archi-set b)))))
       (map (fn [[a b]] [(:bounded-ratio a) (:bounded-ratio b)]))))

#_(def intervals [[5/4 7/4]
                  [21/16 15/8]])

(def intervals [[1 9/8]
                [1 8/7]])

(comment
  (->> [1 3 5 7]
       (cps/make 2)
       :scale
       get-cps-diads
       (map (fn [intervals*]
              {:intervals intervals*
               :ratio (apply interval intervals*)
               :cents (convo/ratio->cents (apply interval intervals*))}))
       (sort-by :cents))
  (low :freq (* 250) :atk 3 :dcy 10 :amp 1.5)
  (low :freq (* 250 9/8) :atk 2 :dcy 7 :amp 1.3)
  (low :freq (* 250 8/7) :atk 2 :dcy 7 :amp 1.3))

(comment
  (gp/stop)
  (ref-rain
   :id ::loop
   :durs (let [range* (range 10)]
           (interleave (map (fn [_] (rrand 1 60)) range*)
                       (repeat 2)))
   :on-event (on-event
              (let [intervals* (rand-nth intervals)
                    octave (rand-nth [1/4 1/2 1 2 4])]

                (println intervals*
                         #_octave
                         (apply interval intervals*)
                         (convo/ratio->cents (apply interval intervals*)))
                ((rand-nth [melody chord])
                 :base-freq (* octave 440)
                 :intervals intervals*
                 :dur dur
                 :synth (rand-nth [low short-plate #_it-synth]))))))

