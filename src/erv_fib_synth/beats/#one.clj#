(ns erv-fib-synth.beats.one
  (:require [erv-fib-synth.midi :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [overtone.core :as o :refer :all :exclude [on-event scale]]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]
            [clojure.string :as str]))

(def scale
  (->> [1 3 5 7]
       (cps/->cps 2)
       cps/set->maps
       (cps/bound-ratio 4)
       (cps/maps->data :bounded-ratio)
       :scale
       #_ (#(cps/filter-scale % #{7}))))

(do
  (defn hexarhythm
    ([gens sequence*] (hexarhythm gens sequence* 0 [1]))
    ([gens sequence* offset] (hexarhythm gens sequence* offset [1]))
    ([gens sequence* offset mults]
     (->> [1 3 5 7]
          (cps/->cps 2)
          cps/set->maps
          (cps/bound-ratio 4)
          (cps/maps->data :bounded-ratio)
          :scale
          (#(cps/filter-scale % gens))
          (map :bounded-ratio)
          (sort)
          (o/rotate offset)
          ((fn [ratios] (map #(wrap-at % ratios) sequence*)))
          (map-indexed #(* %2 (wrap-at %1 mults))))))
  (println (hexarhythm #{1 3 5 7} [0 2 2 1 2 0 1] 4 [1])))



(def melo (scale/stateful-interval->degree 0))
(scale/deg->freq scale 60 (melo (rand-nth [1 -2])))

(-> scale)

(defn m
  ([index intervals ] (m index intervals 30))
  ([index intervals bound]
   (scale/deg->freq scale 100
                    (mod (melo (wrap-at index intervals)) bound))))

(defn r [deg] (->> scale (wrap-at deg) :bounded-ratio))

(comment
  :boom
  ((o/synth (o/out 0 (-> (o/play-buf 2 (ps 99))
                         (#(+ % (free-verb % 0.5 2)))
                         (* 5)
                         (distort)
                         (* (o/env-gen (o/env-perc 0.1 8) :action o/FREE)))))))

(comment
  ((o/synth (o/out 0 (-> (o/play-buf 2 (ps 19))
                         (* (o/env-gen (o/env-perc 1 2) :action o/FREE))
                         (pan2 1)))))
  (o/stop))

(do
  (defn period [seconds durs]
    (let [ratio (/ seconds (apply + durs))]
      (mapv #(* ratio %) durs)))
  (period 1 [1 2])
  (defn periods [seconds & durs]
    (mapcat (partial period seconds) durs))
  (periods 1 [1] [1]))

(do
  (defsynth perc* [buf 0
                   rate 1
                   amp 0.5
                   pan 0
                   atk 0.05
                   rel 0.5]
    (out 0 (-> (play-buf 2 buf rate)
               (* amp (o/env-gen (o/env-perc atk rel) :action o/FREE))
               (pan2 pan))))
  (comment (perc* (kicks 5) :amp 0.4 :atk 0.1 :rel 0.3)))

(defsynth lo [freq 100
              freq2 100
              freq3 100
              amp 0.1
              atk 0.1
              dcy 0.5
              out 0]
  (let [sig (-> (lf-tri freq)
                (+ (* 0.6 (saw freq2) (sin-osc freq3)))
                (lpf 12000)
                (* amp 1.5 (lf-noise1 0.1) (o/env-gen (o/env-perc atk dcy) :action o/FREE)))]
    (o/out out sig)))

(defsynth sin* [freq 100
                freq2 100
                freq3 100
                amp 0.1
                atk 0.1
                dcy 0.5
                out 0]
  (let [sig (-> (sin-osc freq)
                (lpf 12000)
                (* amp 1.5 (lf-noise1 0.1) (o/env-gen (o/env-perc atk dcy) :action o/FREE)))]
    (o/out out sig)))

(defsynth reverb [in 0 room-min 0.3 room-max 3]
  (o/out 0
         (-> (o/in in)
             (pan2 (lf-noise1:kr 0.5))
             (free-verb (lf-noise1:kr 0.1)
                        (-> (lf-noise1:kr 0.5) (range-lin:kr room-min room-max))))))

(defonce main-g (group "get-on-the-bus main"))

(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))

(map
 #(scale/deg->freq scale 60 (melo (wrap-at % [1 -2 1])))
 (range 20))

(comment
  (lo)
  (gp/stop)
  (def kicks (o/load-samples "/home/diego/Downloads/Cymatics x S1 - Artist Series Sample Pack/Kicks/*.wav"))
  (def snares (o/load-samples "/home/diego/Downloads/Cymatics x S1 - Artist Series Sample Pack/Snares/*.wav"))
  (def hh (o/load-samples "/home/diego/Downloads/Cymatics x S1 - Artist Series Sample Pack/Hihats/*.wav"))
  (def ps (o/load-samples "/home/diego/Downloads/Cymatics x S1 - Artist Series Sample Pack/Percussion/*.wav"))
  (def reverbs
    (->> (range 10)
         (mapv (fn [_] (let [bus (audio-bus)]
                        {:fx (reverb [:tail later-g] :in bus)
                         :bus bus})))))
  (->> snares (map (juxt :id :name)) #_(filter #(-> % second (str/includes? "CONGA"))) println)
  (->> hh (map (juxt :id :name)) #_(filter #(-> % second (str/includes? "CONGA"))) println)
  (ref-rain :id ::b
            :durs (periods 1

                           #_(hexarhythm #{1 3 5 7} [1 1 2])
                           #_(hexarhythm #{1 3 5 7} [1 1 2])
                           #_(hexarhythm #{1 3 5 7} [1 1 2])
                           #_(hexarhythm #{1 3 5 7} [1 3])
                           #_(hexarhythm #{1 3 5 7} [1 1 2])
                           #_(hexarhythm #{1 3 5 7} [1 1 2])
                           #_(hexarhythm #{1 5 7} [1 1 2])
                           #_(hexarhythm #{1 5 7} [1 1 2])
                           #_(hexarhythm #{1 5 7} [1 1 2])
                           #_(hexarhythm #{1 5 7} [2 2 3])
                           (hexarhythm #{1 7 3} [1 2 3 4 2])
                           (hexarhythm #{1 3 5 7} [2 3 0]))
            :on-event
            (on-event (case (wrap-at index [0 0 1 2 0 1])
                        0 (perc* (kicks 5) :amp (wrap-at index [0.7 0.35 0.4 0.3]) :atk 0.1 :rel 0.3)
                        1 (perc* (snares 6) :amp 0.2 :atk 0.7 :rel 0.5)
                        2 (do
                            (perc* (snares 5) :amp 0.1 :atk 0.5 :rel 0.5)
                            (perc* (hh 5) :amp 0.3 :atk 0.1 :rel 2))
                        3 (do
                            (perc* (kicks 5) :amp 0.3 :atk 0.1 :rel 0.3)
                            (perc* (snares 5) :amp 0.1 :atk 0.5 :rel 0.5)
                            (perc* (hh 5) :amp 0.3 :atk 0.1 :rel 2))
                        nil)))
  (ref-rain :id ::c
            :durs (periods 1 (hexarhythm #{1 3 5 7} [4 3 4 3 4 3 4 6]))
            :on-event
            (on-event (case (wrap-at index (concat (repeat 5 19)
                                                   [5 0 2 0 5 0]
                                                   (repeat 5 21)
                                                   [1 1 1 1 2]))
                        0 (perc* (ps 6) :amp 0.3 :atk 0.4 :rel 0.8)
                        1 (perc* (ps 3) :amp 0.3 :atk 0.4 :rel 0.8)
                        2 (perc* (ps 8) :rate (wrap-at index [0.5 0.6 0.3 0.2]) :amp 0.1 :atk 1 :rel 0.8)
                        nil)))

  (ref-rain :id ::b19
            :durs (periods 2 (hexarhythm #{1 3 7}  [0 1 0 2 0 1 0 1 0 1 0 1]))
            :on-event
            (on-event
             (let [scale (cps/filter-scale scale (wrap-at index
                                                          [#{1 7 3 5}
                                                           ;; #{3 5} #{1} #{1 3} #{3}
                                                           ;; #{5} #{7 5} #{1} #{3}
                                                           ]))
                   f (scale/deg->freq scale (rand-nth [400])
                                      (wrap-at index [0 1 -1 0 1 -2 0 1 -5]))
                   f2 (scale/deg->freq scale 200 (wrap-at index [0 -1]))
                   f3 (scale/deg->freq scale 100 (wrap-at index [0]))
                   atk (+ 0.5)
                   dcy (+ 0.5)
                   amp (wrap-at index [0.03 0.02])
                   rv-bus (:bus (rand-nth reverbs))]
               (case (wrap-at index [0 0 0 1])
                 0 (sin* [:tail early-g] f f2 f3
                         :amp (+ 0.1 (rand 0.03))
                       :atk 0.1 :dcy 0.3 :out rv-bus)
                 1 (lo [:tail early-g] f2 f f3
                       :amp (+ 0.05)
                       :atk atk :dcy dcy :out rv-bus)
                 nil)))))

(comment
  (ref-rain :id ::c
            :durs (periods 1
                           #_(flatten (repeat 4 [1 1]))
                           (hexarhythm #{1 7 3 5 9}
                                       (flatten (repeat 4 [1 5])) 0))
            :on-event
            (on-event (case (wrap-at index [0 0 1 2 0 1 0 2])
                        0 (perc* (wrap-at index [(kicks 2)
                                                 (kicks 2)
                                                 (kicks 0)
                                                 (kicks 8)
                                                 (kicks 7)
                                                 (kicks 9)
                                                 (kicks 9)])
                                 :amp (wrap-at index (map #(+ 0.051 %)
                                                          [0.1 0.35 0.4 0.3]))
                                 :atk 0.4 :rel 0.1)
                        1 (perc* (snares 6) :amp 0.2 :atk 0.7 :rel 0.5)
                        2 (do
                            (perc* (snares 5) :amp 0.1 :atk 0.5 :rel 0.5)
                            (perc* (hh 5) :amp 0.3 :atk 0.1 :rel 2))
                        3 (do
                            (perc* (kicks 5) :amp 0.3 :atk 0.1 :rel 0.3)
                            (perc* (snares 5) :amp 0.1 :atk 0.5 :rel 0.5)
                            (perc* (hh 5) :amp 0.3 :atk 0.1 :rel 2))
                        nil))))
