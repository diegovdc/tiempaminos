(ns tieminos.lc.barcelona-2022-08
  (:require
   [overtone.midi :as midi]
   [tieminos.midi.algo-note :refer [algo-note]]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.midi.mpe
    :refer [all-notes-off get-cps-pitch-class mpe-note-off mpe-note-on]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  "Usage"
  (require '[tieminos.midi.core :refer [midi-in-event]]
           '[tieminos.midi.mpe :refer [mpe-note-on mpe-note-off]]
           '[erv.cps.core :as cps])
  (def outy (midi/midi-out "VirMIDI"))
  (def hex (cps/make 2 [1 13 15 7]))
  (all-notes-off outy)
  (mpe-note-on :sink outy
               :scale (:scale hex)
               :base-freq 30
               :get-pitch-class get-cps-pitch-class
               :deg-offset 0
               :midi-note 4
               :vel 60)
  (mpe-note-off outy 4)

  (algo-note :sink outy
             :dur 120 ;; milliseconds
             :scale (:scale hex)
             :base-freq 200
             :get-pitch-class get-cps-pitch-class
             :deg-offset 0
             :midi-note 4
             :vel 60)
  (gp/stop)
  (ref-rain :id :bd
            :durs [3 2 3 3 2]
            :tempo 131
            :ratio 1/8
            :on-event (on-event
                       (algo-note :sink outy
                                  :dur (rrand 80 90) ;; milliseconds
                                  :scale (:scale hex)
                                  :base-freq 30
                                  :get-pitch-class get-cps-pitch-class
                                  :deg-offset 0
                                  :midi-note [(at-index [2 10 2 2 4 2 3 5])
                                              #_(at-index [8 10 0 0 8 13 14 11 12])]
                                  :vel (at-index [80 127 60 40]))))
  #_(ref-rain :id :bongo
              :durs [3 2 3 3 2 3 2]
              :tempo 131
              :ratio 1/8
              :on-event (on-event
                         (do (println (mod index 2))
                             (case (mod index 3)
                               0 (algo-note :sink outy
                                            :dur (rrand 200 300) ;; milliseconds
                                            :scale (:scale hex)
                                            :base-freq 120
                                            :get-pitch-class get-cps-pitch-class
                                            :deg-offset 0
                                            :midi-note [(at-index [6 7 8 9 10])]
                                            :vel (at-index [120]))
                               nil?)))))
