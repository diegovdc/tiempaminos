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
  (def out-1 (midi/midi-out "VirMIDI"))
  (def out-2 (midi/midi-out "VirMIDI 2"))
  (def hex (cps/make 2 [11 13 15 17]))
  (all-notes-off out-1)
  (mpe-note-on :sink out-2
               :scale (:scale hex)
               :base-freq 30
               :get-pitch-class get-cps-pitch-class
               :deg-offset 0
               :midi-note 4
               :vel 10)
  (mpe-note-off out-2 4)

  (algo-note :sink out-1
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
                       (algo-note :sink out-1
                                  :dur (rrand 80 90) ;; milliseconds
                                  :scale (:scale hex)
                                  :base-freq 30
                                  :get-pitch-class get-cps-pitch-class
                                  :deg-offset 0
                                  :midi-note [(at-index [2 10 2 2 4 2 3 5])
                                              #_(at-index [8 10 0 0 8 13 14 11 12])]
                                  :vel (at-index [80 127 60 120 40]))))
  (ref-rain :id :pad
            :durs [3 2 3 3 2 3 2]
            :tempo 131
            :ratio 10
            :on-event (on-event
                       (do #_(println dur-ms)
                           (algo-note :sink out-2
                                      :dur dur-ms
                                      :scale (:scale hex)
                                      :base-freq 120
                                      :get-pitch-class get-cps-pitch-class
                                      :deg-offset -6
                                      :midi-note [(at-index [7 4 8 6 9 10 13 5 11])
                                                  #_(at-index [17 18 19 10])]
                                      :vel (at-index [40 120 40 90])))))
  (ref-rain :id :pad-2
            :durs [3 2 2 4 2 3 2]
            :tempo 131
            :ratio 6
            :on-event (on-event
                       (do #_(println dur-ms)
                           (algo-note :sink out-2
                                      :dur dur-ms
                                      :scale (:scale hex)
                                      :base-freq 120
                                      :get-pitch-class get-cps-pitch-class
                                      :deg-offset 8
                                      :midi-note [(at-index [6 10 14 11 3 2])
                                                  (at-index [10 5 11 3 2 15 5])]
                                      :vel (at-index [120]))))))
