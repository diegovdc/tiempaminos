(ns tieminos.beats.two
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [overtone.core :as o :refer :all :exclude [on-event]]
   [tieminos.osc.core :refer [midi-event]]
   [tieminos.synths :as synth]
   [tieminos.utils :as utils :refer [gen-chord linexp periods]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [wrap-at]]))

(def hex1 (:scale (cps/make 2 [1 23 13 5 7])))

(defsynth sin* [freq 200 amp 0.5 atk 0.1 dcy 0.5]
  (o/out 0 (-> (o/sin-osc freq)
               (pan2)
               (* 0.5 amp (o/env-gen (o/perc atk dcy) :action o/FREE)))))

(scale/deg->freq hex1 200 -1)

(def melodies [[0 1 2 1 0 1 -1]
               [0 1 2 4 6 4 2 3 0]
               [4 5 4 2 3 2 1 2 1 -1 -1 0 1 0 -1 2]
               [4 5 4 1 2 1 0 1 0 -2 -2 -1 1 0 -2 1] ;; variation of the above (not in recording)
               {:notes [-3 5 3 2 0 0 0 2 0 -1]
                :durs [1.333 3 2/3 3/4 2 1.25 1 1 2]}

               ])
(comment
  (midi-event
   :note-on (fn [msg]
              (let [degree (- (:note msg) 36)
                    vel (:velocity msg)]
                (synth/low2 (scale/deg->freq hex1 200  degree)
                            :atk 0.5
                            :dcy 0.3
                            :sust (linexp  1 127 0.8 0.01 vel)
                            :rel 2
                            :amp (linexp  1 127 0.01 0.7 vel)))))








  (o/stop))
(comment
  (def s (synth/low 200 :dcy 10))
  (o/ctl s :freq 800 ))
(comment
  (o/recording-start "/home/diego/Desktop/canto.wav")
  (o/recording-stop)
  (gp/stop )

  (let [melody (->> [-4 3 1 0 -1 -1 -1 0 -1]
                    (map #(scale/deg->freq hex1 400 %)))]
    (gp/stop )
    (ref-rain :id ::a
              :durs (periods 5 [1.333 3 2/3 3/4 2 1.25 1 1 2])
              :on-event
              (on-event (synth/low
                         (wrap-at index melody)
                         :amp 0.01
                         :dcy 1))))
  (let [melody (->> [0 -10 0 -11 1 -12 2]
                    (map #(scale/deg->freq hex1 100 %)))]
    (ref-rain :id ::c
              :durs (periods 4 [2 2])
              :on-event
              (on-event (synth/low (wrap-at index melody)
                                   :amp 0.4
                                   :dcy 4))))
  (let [melody (->> [#_[#{5} [5 0]]
                     #_[#{5 13} [14 1]]
                     #_[#{5 13 23} [10 13 15 17]]
                     #_[#{7 23} [5 15]]
                     [#{13 23} [-5 7 21]]
                     #_[#{7 13} [3  20]]
                     [#{5 13 23} [10 13 15 17]]
                     [#{ 23} [11]]
                     #_[#{5 13 23} [10 13 15 17]]
                     #_[#{13 23} [5 9]]
                     [#{5 13} [0 20 13 14]]
                     #_[#{5 13} [0 21 13 14]]
                     [#{5 13} [3 22]]
                     #_[#{7 1 23} [5]]
                     [#{1 13} [1 2 3]]
                     #_[#{7 23} [12 15]]
                     ]
                    #_(take 2)
                    (mapv (partial gen-chord hex1 200)))]
    (ref-rain :id ::g
              :ref ::c
              :durs (periods 2
                             #_[1 2 1 8]
                             [1 2 1 8]

                             )
              :loop? true
              :on-event
              (on-event (let [notes   (wrap-at index melody)
                              harmony (drop-last 1 notes)]

                          (println (last (wrap-at (- 3 index) melody)))

                          (when (> 0.3 (rand))
                            #_(synth/low
                               (* 3/2 (last (sort (wrap-at (- index (rand-int 2))  melody))))
                               :amp 0.03
                               :atk 2
                               :dcy (+ 1 (* 3 dur))))
                          (doseq [n harmony]
                            (synth/short-plate
                             n
                             :mod-freq 6000
                             :amp 0.04
                             :atk 0.1
                             :dcy (* 2 dur)))
                          (synth/low
                           (last notes)
                           :amp 0.03
                           :atk 0.1
                           :dcy (+ 1 (* 3 dur)))))))
  )
