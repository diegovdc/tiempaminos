(ns erv-fib-synth.lc-fam.two
  (:require [erv-fib-synth.midi :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [overtone.core :as o :refer :all :exclude [on-event scale]]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]
            [clojure.string :as str]))

(def ps (o/load-samples "/home/diego/sc/taller-topologias-temporales-2019/Synths/drumKits/IAMM_Kits/**/*.wav"))
(->> ps (map (juxt :id :name)) #_(filter #(-> % second (str/includes? "CONGA"))) println)



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

(def e-bd (ps 99))
(def bd (ps 51))
(def cabasa (ps 4))
(def e-cabasa (ps 62))
(def hi-conga (ps 14))
(def low-conga (ps 14))                 ;; pass a 0.7 rate
(comment
  ((o/synth (o/out 0 (-> (o/play-buf 2 (ps 19))
                         (* (o/env-gen (o/env-perc 1 2) :action o/FREE))
                         (pan2 1)))))
  (o/stop))

(defn period [seconds durs]
  (let [ratio (/ seconds (apply + durs))]
    (mapv #(* ratio %) durs)))

(defsynth perc* [perc bd
                 rate 1
                 amp 1
                 pan 0
                 mix 0.3
                 room 1]
  (out 0 (-> (play-buf 2 perc rate)
             (* amp (o/env-gen (o/env-perc 1 2) :action o/FREE))
             (free-verb mix room)
             (pan2 pan))))

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

(o/stop)
#_(lo 500)
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
  (gp/stop)
  (def reverbs
    (->> (range 10)
         (mapv (fn [_] (let [bus (audio-bus)]
                        {:fx (reverb [:tail later-g] :in bus)
                         :bus bus})))))
  (doseq [r reverbs] (ctl (:fx r) :room-min 2.5))
  (ref-rain :id ::aa2
            :durs (hexarhythm #{3} [2 0 0 0 0 0 0 0 1 0] 0 [1/2])
            :on-event (on-event (do #_(case (wrap-at index [1 1 0])
                                        0 (perc*
                                           :perc (wrap-at index [cabasa])
                                           :room 1
                                           :amp (wrap-at index [0.5 1]))
                                        nil
                                        ))))

  (ref-rain :id ::b19
            :durs (period 0.2 (hexarhythm #{1 7 3} [5 2 1] 1 [5 2 2 3 3 2]))
            :on-event
            (on-event
             (let [scale (cps/filter-scale scale (wrap-at index
                                                          [#{7 1 5 3}
                                                           #{5 7}
                                                           #{5 1 3}
                                                           #{1} #{7} #{1}
                                                           ;; #{3 5} #{1} #{1 3} #{3}
                                                           ;; #{5} #{7 5} #{1} #{3}
                                                           ]))
                   f (scale/deg->freq scale (rand-nth [99 299 399])
                                      (wrap-at index [0 1 3 1 4 6]))
                   f2 (scale/deg->freq scale 20 (wrap-at index [1 13 -1 3 2]))
                   f3 (scale/deg->freq scale 100 (wrap-at index [1 0 -2 7]))
                   atk (+ 0.01 (rand 5))
                   dcy (+ 0.1 (rand 8))
                   amp 0.03
                   rv-bus (:bus (rand-nth reverbs))]
               (case (wrap-at index [,, 4  2 4
                                     ,, 4 4
                                     ;; 0 1 1 2 0 2 2 0 2
                                     ])
                 0 (lo [:tail early-g] f f2 f3
                       :amp (+ amp (rand 0.1))
                       :atk atk :dcy dcy :out rv-bus)
                 1 (lo [:tail early-g] f2 f f3
                       :amp (+ amp (rand 0.2))
                       :atk atk :dcy dcy :out rv-bus)
                 2 (lo [:tail early-g] f3 f2 f
                       :amp (+ amp (rand 0.1))
                       :atk atk :dcy dcy :out rv-bus)
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
