(ns tieminos.lc-fam.three
  (:require [clojure.string :as str]
            [tieminos.midi.core :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [overtone.core :as o :refer :all :exclude [on-event]]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]))
(comment
  (def desert-at-night (o/load-sample "~/Downloads/138288__kangaroovindaloo__desert-at-night.aiff")))

(do
  (defn hexarhythm
    ([gens sequence*] (hexarhythm gens sequence* 0 [1]))
    ([gens sequence* offset] (hexarhythm gens sequence* offset [1]))
    ([gens sequence* offset mults]
     (->> [1 3 5 7 11]
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
  #_(println (hexarhythm #{1 3 5 7} [0 2 2 1 2 0 1] 4 [1])))

(defn period [seconds durs]
  (let [ratio (/ seconds (apply + durs))]
    (mapv #(* ratio %) durs)))

(defsynth field [sample 1
                 freq 300
                 dur 5
                 amp 1
                 mod* 9
                 pan 0
                 rate 1
                 start-pos 0]
  (o/out 0 (-> (* (play-buf 2 sample rate :start-pos start-pos)
                  (sin-osc freq :mul 0.3))
               (* amp
                  #_(lf-tri (* mod*) :mul 0.001)
                  (env-gen (envelope [0 1 0.5 0.5 0] [2 0.1 dur 2]) :action o/FREE))
               (pan2 (lf-noise1 (lf-noise1 (range-lin (lf-noise1 2) 0.1 2)))))))

(comment (field desert-at-night 0.1 :amp 4 :rate 0.2 :start-pos (* 44100 (rand-int (:duration desert-at-night)))))

(defsynth sin* [freq 300
                mod* 300
                dur 5
                amp 0.1]
  (o/out 0 (-> (+ (*  (range-lin (lf-noise1 (/ mod* 100)) 0 1) (sin-osc freq))
                  (* 0.05 (pulse [(* 2 freq)
                                  (* 3 (+ 1 freq))
                                  (* 4 (+ 0.1 freq))
                                  (* 7 (+ 0.1 freq))]))
                  (* (range-lin (lf-noise1 (/ mod* 10)) 0 0.2)
                     (sin-osc (range-lin (pulse (* mod* 3)) 100 freq))))
               (* amp 15
                  [(lf-tri 0.1)
                   (* 0.1 (lf-tri (range-lin (lf-noise1 2) 0 120)))]
                  (env-gen
                   (envelope
                    [0 0 1 0.5 0.3 0]
                    [2 (+ 1 (/ dur 10)) (+ 0.1 (/ dur 20)) dur 0.1])
                   :action o/FREE))
               mix
               (pan2 (lf-noise1 0.2)))))

(def hex (hexarhythm #{1 5 3} [1 2 2] 0 [1 1 1 1/2]))

(comment
  (o/stop)
  (demo (* (lpf [(sin-osc 200)
                 (sin-osc 500)
                 (sin-osc 400)])
           (env-gen (env-perc)))))

(comment
  (gp/stop)

  (ref-rain :id ::b14
            :durs (period 3 (hexarhythm #{1 11 3} [0 2 5 0 2 5 0 1 7]))
            :on-event
            (on-event
             (let [scale (->> [1 13 51 7 11 17 19]
                              (cps/->cps 2)
                              cps/set->maps
                              (cps/bound-ratio 2)
                              (cps/maps->data :bounded-ratio)
                              :scale
                              (#(cps/filter-scale % (wrap-at index
                                                             [#{1 7}
                                                              #_#{1 19 17}
                                                              #_#{13 51 19}]))))
                   deg (- (rand 40) 10)
                   freq* (scale/deg->freq scale 1 deg)
                   freq (scale/deg->freq scale 300 deg)
                   dur (+ 3 (rand 5))]
               (case (wrap-at index [1])
                 0 (field desert-at-night
                          0.1
                          :mod freq*
                          :freq freq
                          :dur dur
                          :amp (rand 5)
                          :rate (/ freq* 2)
                          :start-pos (* 44100 (rand (:duration desert-at-night))))
                 1 (sin* freq freq* dur (rand 0.05))
                 nil)))))
