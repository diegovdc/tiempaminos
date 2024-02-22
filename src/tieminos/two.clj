(ns tieminos.two
  (:require [tieminos.midi.core :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [overtone.core :as o]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]))

(def ps #_(o/load-samples "/home/diego/sc/taller-topologias-temporales-2019/Synths/drumKits/IAMM_Kits/**/*.wav"))

(def scale
  (->> [3 5 8 13 51 44 63]
       (cps/->cps 3)
       cps/set->maps
       (cps/bound-ratio 4)
       (cps/maps->data :bounded-ratio)
       :scale
       #_(#(cps/filter-scale % #{7}))))

(o/defsynth sip [freq 400 amp 0.5 pulse 1]
  (o/out 0 (o/pan2 (-> (o/mix [(o/sin-osc freq)
                               (* 1 (o/sin-osc freq) (o/saw freq))])
                       (* amp 0.5
                          #_(o/square pulse)
                          (o/env-gen (o/env-perc 1 2) :action o/FREE))))))

(comment
  (note-on (fn [n]
             (println "deg" (:note n) (:velocity n))
             (sip (scale/deg->freq scale 200 (+ 5 (:note n)))
                  :amp (+ 1 (* 2 (/ (:velocity n) 127)))))))

(+ 1 1)

(def melo (scale/stateful-interval->degree 0))

(defn m
  ([index intervals] (m index intervals 30))
  ([index intervals bound]
   (scale/deg->freq scale 100
                    (mod (melo (wrap-at index intervals)) bound))))

(mod (melo 2) 50)

(defn r [deg] (->> scale (wrap-at deg) :bounded-ratio))

(o/defsynth perc [buf 0, rate 1, amp 0.5]
  (o/out 0 (* amp (o/pan2 (o/play-buf:ar 2 buf rate 1 :action 2)
                          0))))

(o/defsynth sipy [freq 400 amp 0.5 pulse 1]
  (o/out 0 (-> (o/mix [(o/square freq)
                       (* 1 (o/sin-osc freq) (o/saw freq))])
               (o/pan2)
               (* amp 0.3
                  (o/square pulse)
                  (o/env-gen (o/env-perc 1 3) :action o/FREE)))))
(o/defsynth sip2 [freq 400 freq2 400 amp 0.5 pulse 1]
  (o/out 0 (-> (o/mix [(o/sin-osc freq)
                       (* 0.1 (o/sin-osc freq2) (o/saw freq))])
               (* amp 0.3
                  #_(o/square pulse)
                  (o/sin-osc freq2)
                  (o/env-gen (o/env-perc 4 5) :action o/FREE)))))

(comment

  (sipy)
  (ref-rain :id ::eee
            :ref ::aa
            :durs [1/4 1/4 1/5]
            :ratio 1/6
            :on-event (on-event (do (sipy (m index [1 2 -1 -1 1])
                                          :amp 0.5
                                          :pulse 5))))
  (ref-rain :id ::ee
            :ref ::aa
            :durs [2 3 9]
            :ratio 1/6
            :on-event (on-event (do #_(sip2 (m index [1 -2 5 -11 1 1 1] 100)
                                            (m index [1 -2 5 10 11 1 1 1 1] 80)
                                            :pulse (* 2 (r index))))))

  (ref-rain :id ::f
            :ref ::ee
            :durs [1 2 3 1/3 5]
            :ratio 1/10
            :on-event (on-event (do
                                  #_(case (wrap-at index [0 1 1 0 1 1])
                                      0 (sip (m index [1 -2 5 7 7 1 1 1 1 1] 70)
                                             :pulse (* 2 (r index)))
                                      1  (sip2 (m index [1  -5 1 1 1] 35)
                                               (m index [1  5 -10  20] 35)
                                               :pulse (* 2 (r index)))
                                      nil))))

  (ref-rain :id ::aaa
            ::ref ::ee
            :durs [1]
            :ratio 1/5
            :on-event (on-event
                       (do #_(case (wrap-at index [2 0 1 1])
                               0 (perc (ps 9)
                                       :rate (r index)
                                       :amp (wrap-at 1 [0.5 0.3]))
                               1 (perc (ps 3)
                                       :rate (r (wrap-at index [1 7 5]))
                                       :amp (wrap-at 1 [0.5 0.3]))
                               2 (perc (ps 20)
                                       :rate (r (wrap-at index [1 7]))
                                       :amp (wrap-at 1 [0.5]))
                               nil))))
  (ref-rain :id ::bbb
            :ref ::aaa
            :durs [1 1/2 1 1/7 1 1/5]
            :ratio 1/3
            :on-event (on-event
                       (do #_(case (wrap-at index [0 1 0 1 0])
                               0 (perc (ps 10)
                                       :rate (r (wrap-at index [1 5 8]))
                                       :amp (wrap-at 1 [0.5 0.3 2]))
                               1 (do (perc (ps 3)
                                           :rate (r (wrap-at index [1 5 8]))
                                           :amp (wrap-at 1 [1.5 0.4]))
                                     (perc (ps 13)
                                           :rate (r (wrap-at index [1 5 8]))
                                           :amp (wrap-at 1 [0.2])))
                               nil))))

  (o/stop))
