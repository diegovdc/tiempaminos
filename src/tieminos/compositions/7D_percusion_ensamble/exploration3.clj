(ns tieminos.compositions.7D-percusion-ensamble.exploration3
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.midi.core :refer [all-notes-off get-iac2!]]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.polydori.analysis.dorian-hexanies :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [map-subscale-degs rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.standard :refer [rrand]]))

(def sink (midi/midi-out "VirMIDI"))

(def iac2 (get-iac2!))

(defn bh
  "Blackhole outs (starts on chan 3)"
  [out]
  (+ 22 out))

(defn my-malgo
  [config]
  (malgo-note (merge {:sink sink
                      :scale-size 29
                      :base-midi-deg 60
                      :base-midi-chan 0}
                     config)))

(defn- deg->freq [& {:keys [base-freq scale degree]}]
  (scale/deg->freq (:scale polydori-v2)
                   base-freq
                   (map-subscale-degs (count (:scale polydori-v2))
                                      (:degrees
                                       (nth
                                         dorian-hexanies-in-polydori
                                         scale))
                                      degree)))


(defn diat->polydori-degree
  [scale degree]
  (map-subscale-degs (count (:scale polydori-v2))
                     (:degrees
                      (nth
                        dorian-hexanies-in-polydori
                        scale))
                     degree))

(o/defsynth low
  [freq 85
   amp 0.5
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 15) (+ freq 15))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(o/defsynth short-plate
  [freq 200
   amp 0.5
   mod-freq 1000
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 200) (+ freq 200))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(def synths (map #(partial % (groups/early)) [low short-plate]))

(defn init! []
  (groups/init-groups!))

(def mempan
  (memoize (fn [deg-mod]
             (rrand -1.0 1))))

(comment
  (init!)
  (gp/stop)
  (all-notes-off sink)
;;;;;;;;;;;;
;;; INTRO
;;;;;;;;;;;;;
  (init!)
  
  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :pan (mempan (mod deg 2))
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  #_(my-malgo {:deg (diat->polydori-degree 0 deg) :dur 0.1 :vel 100}))))

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :pan (mempan (mod deg 2))
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  #_(when (> (rand ) 0.6)
                      (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [4 -4 3 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100})))))

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 #_3]) #_ 4])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :pan (mempan (mod deg 2))
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  ;; TODO agregar silencions con when-not
                  #_(when (> (rand ) 0.6)
                      (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                  #_(if (> (rand) 0.5)
                      (my-malgo {:base-midi-chan 0 :deg (diat->polydori-degree 0 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                  #_(when (> (rand) 0.4))
                  #_(my-malgo {:deg (diat->polydori-degree 0 (+ -1 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                  #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  #_(ref-rain
      :id ::1 :durs [3 2 2] :ratio 1/9
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [0 2 1 (at-i [2 3])])]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrange 0.45 0.51)
                      :out (bh 0))
                    #_(my-malgo {:base-midi-chan 0 :deg (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4}) deg) :dur (weighted {0.1 9 1 5}) :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg (+ 6 deg) :dur 0.1 :vel 100}))))



  #_(ref-rain
      :id ::1 :durs [3 2 2] :ratio 1/9
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [0 2 1 (at-i [2 3]) 4])]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrange 0.45 0.51)
                      :out (bh 0))
                    #_(my-malgo {:base-midi-chan 0 :deg (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4}) deg) :dur (weighted {0.1 9 1 5}) :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg (+ 6 deg) :dur 0.1 :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Comineza transición 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 3]) (at-i [7 4]) (at-i [4 5 ]) 3])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :out (bh 0))
                  (when (or true ( #{1 3 5} (mod (inc index) 7)))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  #_(when (or #_true (#{5 10 13} (mod (inc index) 14)))
                      (my-malgo {:deg (diat->polydori-degree 2 (+  (weighted {-6 5 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100}))
                  #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  (ref-rain ;; NOTE evol previous
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                                        ; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
                      deg (at-i #_[(at-i [0 #_ #_5 5])
                                   2
                                   (at-i [1 -7])
                                   (at-i [8  #_ #_2 8])
                                   (at-i [4 7 #_3])
                                   (at-i [6 5])
                                   (at-i [3  #_ #_ #_3 8 9])]
                                [(at-i [0 #_ #_ 5 5])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 #_ #_ 2 8])
                                 (at-i [4 7 #_3])
                                 (at-i [6 5])
                                 (at-i [3  #_ #_ #_3 8 9])])]
                  (when (#{1 2 3 4 5} (mod i 5))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale (at-i [0 0 0 0 0]) :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  (when (or #_true (#{0 } (mod index 7)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [200 100]) :scale (at-i [2]) :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  (my-malgo {:deg (diat->polydori-degree 0 (+ #_6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel 10})
                  (if (> (rand) 0.3)
                    ;; NOTE keep vel 10 at first

                    (my-malgo {:deg (diat->polydori-degree (at-i [2]) (+ 2 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5})
                               ;; `+ 20`, add later ;; original [10 30 60]
                               :vel (+ 50 (at-i [70 60 60 70 50]))})
                    ;; FIXME has a -2 midi note
                    ;; NOTE esta modula... ir agregándola lentamente
                    #_(my-malgo {:deg (diat->polydori-degree (weighted {1 0 6 5})
                                                             (+ 8
                                                                (at-i [-6 -12 6 0 0 0 ]) ;; NOTE add later
                                                                (weighted {0 5 4 4 -4 4}) deg))
                                 :dur (weighted {0.1 3 1 5 2 5})
                                 :vel (+ 20 (at-i [10 30]))}))
                  #_(my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))

  
  (gp/stop ::2)

  ;; NOTE not sure about this
  (ref-rain
    :id ::2 :durs [3 2 2] :ratio 1/9
    :ref ::1
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 5 5])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 2 8])
                                 (at-i [4 7 3])
                                 #_(at-i [6 15 10 6])
                                 (at-i [3 3 8 9])])
                      scale-1 (weighted {9 1})]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :amp 0.3
                    :out (bh (case deg -7 2 0)))
                  (when (or #_true (#{3 6} (mod index 7)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [200 #_100]) :scale 2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  #_(my-malgo {:deg (diat->polydori-degree scale-1 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel 10})
                  #_(if (> (rand) 0.7)
                      ;; NOTE keep vel 10 at first

                      (my-malgo {:deg (diat->polydori-degree 2 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [10 30 60])})
                      ;; FIXME has a -2 midi note
                      #_ (my-malgo {:deg (diat->polydori-degree 1 (+ 7
                                                                     (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                     (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [10 30])}))
                  #_ (my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))


  (ref-rain
    :id ::3
    :durs
    #_[2 1 2 2 2 2 2 2]
    #_[3 3 3 3 3 1 11 1 1 3 3 3 11 3 3 3 3 ]
    #_[3 3 3 3 11 3 1 11 1 1 3 3 11 3 11 3 3 3 3]
    #_[1 1 1 1 1 1 1 11 1 1 11 1 1 1 1 1 1]
    [1]
    :ratio 1/9
    :ref ::1
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (+ 12  (at-i [(at-i [0 5 5])
                                        2
                                        #_(at-i [1 -7])
                                        #_(at-i [8 2 8])
                                        (at-i [4])
                                        (at-i [6 15 10 6])
                                        #_(at-i [3 3 8 9])]))
                      scale-1 (weighted {6 0 9 2 10 2})]
                  (when-not (or #_false (#{0 1 2 3 5 6 8 9 10} (mod i 11)))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.02 0.1)
                      ;; :atk (rrand 0.01 0.3)
                      :dcy (rrand 0.1 2)
                      :pan (rrand -1.0 1)
                      :out (bh (case deg -7 2 0)))))))



  (ref-rain
    :id ::3
    :durs
    #_[2 1 2 2 2 2 2 2]
    #_[3 3 3 3 3 1 11 1 1 3 3 3 11 3 3 3 3 ]
    #_[3 3 3 3 11 3 1 11 1 1 3 3 11 3 11 3 3 3 3]
    #_[1 1 1 1 1 1 1 11 1 1 11 1 1 1 1 1 1]
    [1]
    :ratio 1/9
    :ref ::1
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (+ 12  (at-i [(at-i [0 5 5])
                                        2
                                        #_(at-i [1 -7])
                                        #_(at-i [8 2 8])
                                        (at-i [4])
                                        (at-i [6 15 10 6])
                                        #_(at-i [3 3 8 9])]))
                      scale-1 (weighted {6 0 9 2 10 2})]
                  (when-not (or #_false (#{0 1 2 3 5 6 8 9 10} (mod i 11)))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.02 0.1)
                      ;; :atk (rrand 0.01 0.3)
                      :dcy (rrand 0.1 2)
                      :pan (rrand -1.0 1)
                      :out (bh (case deg -7 2 0)))))))

;;;;;;;;;;;;;;;;
;;;;Second transition
;;;;;;;;;;;;;;;;

  ;; NOTE maybe use before ... still need to figure out pads
  (ref-rain
    :id ::1 :durs [3 2 2 3 2 2 3 2 2 3] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 5])
                                 (at-i [2 ])
                                 #_(at-i [1 -7])
                                 (at-i [8])
                                 #_(at-i [4 7 5])
                                 #_(at-i [6 15 10 6])
                                 #_(at-i [3 3 8 9])])
                      scale-1 (weighted {0 0 6 0 9 7})
                      scale-2 (weighted {2 0 6 4 9 0})]
                  (when (> (rand) 0.3 )
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.1 0.1 #_0.3)
                      ;; NOTE use attack to make padd
                      ;; :atk (rrand 0.01 2)
                      :dcy 3
                      :out (bh (case deg -7 2 0))))
                  (when (or (#{3 6} (mod index 5)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [100 50 400]) :scale scale-2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.1 0.1 #_0.7)
                      ;; :atk (rrand 0.01 5)
                      :dcy 3
                      :out (bh (case deg -7 2 0))))
                  #_(when (> (rand) 0.4 )
                      (my-malgo {:deg (diat->polydori-degree scale-1 (+ 0 (at-i [2 3 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 2 5 3 1}) :vel 100}))
                  #_(when (> (rand) 0.4 )
                      (my-malgo {:deg (diat->polydori-degree scale-2 (+ -2 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 5 1 5 4 2}) :vel 60}))
                  #_(if (> (rand) 0.7)
                      ;; NOTE keep vel 10 at first

                      (my-malgo {:deg (diat->polydori-degree scale-2 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [10 30 60])})
                      ;; FIXME has a -2 midi note
                      #_ (my-malgo {:deg (diat->polydori-degree 1 (+ 7
                                                                     (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                     (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [10 30])}))
                  #_ (my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))
  (gp/stop ::1)


  (gp/stop ::4)

  (let [tr 2] ;; expand let to enclose the other refrain
    (ref-rain
      :id ::2 :durs [3 2 2] :ratio 1/9
      :ref ::1
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [0 5 5])
                                   2
                                   (at-i [1 -7])
                                   (at-i [8 2 8])
                                   (at-i [4 7 5 5 3])
                                   (at-i [6 15 10 6])
                                   (at-i [3 3 8 9])])
                        scale-1 (weighted {4 5 })]
                    (synth
                      :freq (deg->freq :base-freq 100 :scale scale-1 :degree (+ (min 5 tr) deg))
                      :mod-freq (rrand 6000 10000)
                      :amp 0.3
                      :out (bh (case deg -7 2 0)))
                    (synth
                      :freq (deg->freq :base-freq 100 :scale scale-1 :degree (+ tr deg))
                      :mod-freq (rrand 6000 10000)
                      :amp 0.3
                      :out (bh (case deg -7 2 0)))
                    (when (or #_true (#{3 6} (mod index 7)))
                      (synth
                        :freq (deg->freq :base-freq (at-i [100 #_100]) :scale 2 :degree deg)
                        :mod-freq (rrand 6000 10000)
                        :out (bh (case deg -7 2 0))))
                    ))))

  (ref-rain
    :id ::4 :durs [3 2 2] :ratio 1/9
    :ref ::2
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 10 8])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 2 8])
                                 (at-i [4 7 3])
                                 (at-i [6 15 10 6])
                                 (at-i [3 3 8 9])])
                      scale-1 (weighted {4 5})
                      tr (+ #_tr 6)]
                  (if (> (rand) 0.1)
                    ;; NOTE keep vel 10 at first

                    (my-malgo {:deg (diat->polydori-degree 4 (+ tr (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 2 5})
                               :vel (at-i [100 100 100])})
                    ;; FIXME has a -2 midi note
                    (my-malgo {:deg (diat->polydori-degree 4 (+ tr
                                                                (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 2 5})
                               :vel (at-i [100 80])}))
                  )))


  ;; ESTADIO #3
  ;; NOTE construir esto
  ;; [0 5 5] 2 [1 -7] [9 8] [4 -3 7 3] [6 5] [3 3 8 -5 9] [0 5 11]
  ;; estaría chidos unos arpegios ascendentes, quizá en el luma - grabar y repetir loop, en silencio, en el performance subirle para denotar el momento -,
  ;;         algo que construya intensidad
  ;;
  (ref-rain :id ::4 :durs [3 2 2] :ratio 1/9 :ref ::2 :on-event (on-event (let [synth (rand-nth synths) deg (at-i [(at-i [0 5 5]) 2 (at-i [1 -7]) (at-i [9 8]) (at-i [4 -3 7 3]) (at-i [6 5]) (at-i [3 3 8 -5 9]) (at-i [0 5 11])]) scale-1 (weighted {4 5}) tr 0] (synth :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg) :mod-freq (rrand 6000 10000) :dcy 3 :out (bh (case deg -7 2 0))) (if (> (rand) 0.1) (my-malgo {:deg (diat->polydori-degree 4 (+ tr (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [100 100 100])}) (my-malgo {:deg (diat->polydori-degree 4 (+ tr (at-i [-6 -12 6 0 0 0]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [100 80])})))))

  ;; development-exploration of above (same deg seq)
  ;; poco a poco ir introduciendo la scala 5 que en paralelo con el tema actual da un contratema muy bonito y figurativo
  ;; also adding 7 works well
  (ref-rain
    :id ::4 :durs [3 2 2] :ratio 1/9
    :ref ::2
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [
                                 (at-i [0 5 5]) 2
                                 (at-i [1 -7])
                                 (at-i [9 8])
                                 (at-i [4 -3 7 3])
                                 (at-i [6 5])
                                 (at-i [3 3 8 -5 9])
                                 (at-i [0 5 11])])
                      scale-1 (weighted {4 5 5 2})
                      scale-2 (weighted {5 3 7 0})
                      tr 0]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr) )
                    :mod-freq (rrand 6000 10000)
                    :dcy 3
                    :amp 0.5
                    :out (bh (case deg -7 2 0)))
                  (synth
                    :freq (deg->freq :base-freq 400 :scale scale-2 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :dcy (at-i [2 2 1 1 1 2 3])
                    :pan -1
                    :amp 0.5
                    :out (bh (case deg -7 2 0)))
                  (synth
                    :freq (deg->freq :base-freq (at-i [800 400 800 400 800 400 800 400 800 400 800]) :scale 7 :degree (+ 0 deg))
                    :mod-freq (rrand 6000 10000)
                    :dcy (at-i [2 2 1 1 1 2 3])
                    :pan 1
                    :amp 0.5
                    :out (bh (case deg -7 2 0)))
                  (when (#{0 3 5} (mod i 7))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 7 :degree (- deg 2))
                      :mod-freq (rrand 6000 10000)
                      :dcy (at-i [2 2 1 1 1 2 3])
                      :pan 1
                      :amp 0.6
                      :out (bh (case deg -7 2 0))))
                  (if (> (rand) 0.3)
                    (my-malgo {:deg (diat->polydori-degree scale-1
                                                           (+ tr (at-i [2 2 3 2 3])
                                                              (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 2 5})
                               :vel (at-i [60 100 40])})
                    (my-malgo {:deg (diat->polydori-degree scale-2
                                                           (+ tr
                                                              (at-i [-6 6 0 -6 0]) ;; NOTE add later
                                                              (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 })
                               :vel (at-i [100 80])}))
                  )))
  
  :7d-perc)

(comment
  (init!)
  (gp/stop)
  (all-notes-off sink)
  )
