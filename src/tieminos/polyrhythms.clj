(ns tieminos.polyrhythms

  (:require [tieminos.midi.core :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [overtone.core :as o :refer :all :exclude [on-event scale]]
            [time-time.dynacan.players.gen-poly :as rain :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]
            [clojure.string :as str]))
(comment
  (def ps (o/load-samples "/home/diego/sc/taller-topologias-temporales-2019/Synths/drumKits/IAMM_Kits/**/*.wav"))
  (->> ps (map (juxt :id :name)) #_(filter #(-> % second (str/includes? "CONGA"))) println))



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
  #_(println (hexarhythm #{1 3 5 7} [0 2 2 1 2 0 1] 4 [1])))



(def melo (scale/stateful-interval->degree 0))
(melo (wrap-at 2 [1 0 0 0 2]))



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
  (def e-bd (ps 99))
  (def bd (ps 51))
  (def cabasa (ps 4))
  (def e-cabasa (ps 62))
  (def hi-conga (ps 14))
  (def low-conga (ps 14))                 ;; pass a 0.7 rate
  ((o/synth (o/out 0 (-> (o/play-buf 2 (ps 19))
                         (* (o/env-gen (o/env-perc 1 2) :action o/FREE))
                         (pan2 1)))))
  (o/stop))

(defsynth perc* [perc 0
                 rate 1
                 amp 3
                 pan 0
                 mix 0.3
                 room 1]
  (out 0 (-> (play-buf 2 perc rate)
             (* 6 amp (o/env-gen (o/env-perc 1 2) :action o/FREE))
             (free-verb mix room)
             (pan2 pan))))


(defn period [seconds durs]
  (let [ratio (/ seconds (apply + durs))]
    (mapv #(* ratio %) durs)))

(comment
  (rain/stop)
  (ref-rain :id ::a18
            :durs (period 2 (hexarhythm #{3} [0]))
            :tempo 60
            :on-event (on-event (do
                                  (println dur (rand-nth ["uno"] "tres"))
                                  (case (wrap-at index [0])
                                    0 (perc*
                                       :perc (wrap-at index [hi-conga])
                                       :room 1
                                       :amp (wrap-at index [0.5 1]))
                                    nil))))

  (ref-rain :id ::b11
            :durs (period 2 (hexarhythm #{3 5} [0 1 0]))
            :ref ::a8
            :ratio 1
            :on-event (on-event (do (case (wrap-at index [0])
                                      0 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [0.5 0.5 0.5 1])
                                         :rate (wrap-at index [2 4 2 4 2])
                                         :pan (wrap-at index [-0.5 0.3 1])
                                         :amp (wrap-at index [1.2 0.7 0.5]))
                                      1 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [1 0.5])
                                         :rate (wrap-at index [1 1.333 1 1.333 5 3 6])
                                         :pan (wrap-at index [-0.5 0.8 1 0])
                                         :amp (wrap-at index [1.7 2 0.5]))
                                      2 (perc*
                                         :perc e-cabasa
                                         :room 2
                                         :rate (wrap-at index [1.2 2])
                                         :pan (wrap-at index [-1 0.3 1])
                                         :amp (wrap-at index [2 2 3 3]))
                                      3 (perc*
                                         :perc bd
                                         :room 2
                                         :rate (rand-nth [1.2 0.5 0.2])
                                         :pan (wrap-at index [-1 0.3 1 0])
                                         :amp (wrap-at index [1.7 1 0.5 0.1 2]))
                                      nil))))
  (ref-rain :id ::b12
            :durs (period 2 (hexarhythm #{3 5} [0 1 0 0 3]))
            :ref ::a8
            :ratio 1
            :on-event (on-event (do (case (wrap-at index [2])
                                      0 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [0.5 0.5 0.5 1])
                                         ;; :rate (wrap-at index [2 4 2 4 2])
                                         ;; :pan (wrap-at index [-0.5 0.3 1])
                                         :amp (wrap-at index [1.2 0.7 0.5]))
                                      1 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [1 0.5])
                                         :rate (wrap-at index [1 1.333 1 1.333 5 3 6])
                                         :pan (wrap-at index [-0.5 0.8 1 0])
                                         :amp (wrap-at index [1.7 2 0.5]))
                                      2 (perc*
                                         :perc e-cabasa
                                         :room 2
                                         :rate (wrap-at index [1.2 2])
                                         :pan (wrap-at index [-1 0.3 1])
                                         :amp (wrap-at index [2 2 3 3]))
                                      3 (perc*
                                         :perc bd
                                         :room 2
                                         :rate (rand-nth [1.2 0.5 0.2])
                                         :pan (wrap-at index [-1 0.3 1 0])
                                         :amp (wrap-at index [1.7 1 0.5 0.1 2]))
                                      nil))))
  (ref-rain :id ::b13
            :durs (period 2 (hexarhythm #{3 5 7} [0 1 0 0 3 0 4]))
            :ref ::a8
            :ratio 1
            :on-event (on-event (do (case (wrap-at index [1 1 3])
                                      0 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [0.5 0.5 0.5 1])
                                         ;; :rate (wrap-at index [2 4 2 4 2])
                                         ;; :pan (wrap-at index [-0.5 0.3 1])
                                         :amp (wrap-at index [1.2 0.7 0.5]))
                                      1 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [1 0.5])
                                         :rate (wrap-at index [1 1.333 1 1.333 5 3 6])
                                         :pan (wrap-at index [-0.5 0.8 1 0])
                                         :amp (wrap-at index [1.7 2 0.5]))
                                      2 (perc*
                                         :perc e-cabasa
                                         :room 2
                                         :rate (wrap-at index [1.2 2])
                                         :pan (wrap-at index [-1 0.3 1])
                                         :amp (wrap-at index [2 2 3 3]))
                                      3 (perc*
                                         :perc (ps 21)
                                         :room 0.1
                                         :rate (wrap-at index [1 2 1.5])
                                         :pan (wrap-at index [0])
                                         :amp (wrap-at index [20]))
                                      nil))))
  (ref-rain :id ::b14
            :durs (period 2 (hexarhythm #{3 5 7} [0 1 0 0 1 0 1 0 0 1 0] 2))
            :ref ::a8
            :ratio 1
            :on-event (on-event (do (case (wrap-at index [3 3 3 4])
                                      0 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [0.5 0.5 0.5 1])
                                         ;; :rate (wrap-at index [2 4 2 4 2])
                                         ;; :pan (wrap-at index [-0.5 0.3 1])
                                         :amp (wrap-at index [1.2 0.7 0.5]))
                                      1 (perc*
                                         :perc hi-conga
                                         :room (wrap-at index [1 0.5])
                                         :rate (wrap-at index [1 1.333 1 1.333 5 3 6])
                                         :pan (wrap-at index [-0.5 0.8 1 0])
                                         :amp (wrap-at index [1.7 2 0.5]))
                                      2 (perc*
                                         :perc e-cabasa
                                         :room 2
                                         :rate (wrap-at index [1.2 2])
                                         :pan (wrap-at index [-1 0.3 1])
                                         :amp (wrap-at index [2 2 3 3]))
                                      3 (perc*
                                         :perc (ps 22)
                                         :room 0.1
                                         :rate (wrap-at index [1 2 1.5])
                                         :pan (wrap-at index [-1 1])
                                         :amp (wrap-at index [20]))
                                      nil))))


  (ref-rain :id ::g
            :ref ::a5
            ;; :durs [1 2 3 1/3 5]
            :ratio 1/4
            :on-event (on-event (do
                                  (case (wrap-at index [0 1 1 0 1 1])
                                    0 (perc* e-bd
                                             :rate (wrap-at index [1 1 2])
                                             :amp (wrap-at index [0.1 0.1 0.5 3]))
                                    nil)))))
