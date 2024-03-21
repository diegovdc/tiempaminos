(ns tieminos.compositions.7D-percusion-ensamble.exploration-4b
  "Introduces `sub-rain`"
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [bh deg->freq diat->polydori-degree init! mempan my-malgo surge-suave synths sub-rain]]
   [tieminos.midi.core :refer [all-notes-off]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  (init!)
  (gp/stop)
  (all-notes-off surge-suave))

(comment
  :start-section-1

  (let [degs (fn [at-i] (at-i [0 #_#_#_2 0 (at-i [2 #_3]) #_(at-i [#_7 4]) #_#_(at-i [4 5]) 3]))
        bprob 0.
        m1t -1
        m2t 0
        mprob 0.
        mmix 0]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (degs at-i)]
                  (synth
                   :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                   :mod-freq (rrand 6000 10000)
                   :pan (mempan (mod deg 2))
                   ;; :amp (rrange 0.5 0.8)
                   :out (bh 0))

                  #_(when (or #_true (#{1 3} (mod (inc index) 5)))
                      (synth
                       :freq (deg->freq :base-freq 200 :scale 2 :degree deg)
                       :mod-freq (rrand 6000 10000)
                       :amp (rrange 0.5 0.8)
                       :out (bh 0))))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (degs at-i)]
                  (when (> bprob (rand))
                    (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [4 -4 3 -4 -3]) deg))
                               :dur (weighted {1 9 1.5 8})
                               :vel 100})))))
    (ref-rain
     :id ::1-m :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (+ (degs at-i)
                             (at-i [2 2 3 2 3])
                             (weighted {0 5 4 4 8 3}))]
                  (when (> mprob (rand))
                    (if (> mmix (rand))
                      (my-malgo {:deg (diat->polydori-degree 2 (+ m2t deg))
                                 :dur (weighted {0.1 9 1 5})
                                 :vel 80})
                      (my-malgo {:deg (diat->polydori-degree 0 (+ m1t  deg))
                                 :dur (weighted {0.1 9 1 5})
                                 :vel 100})))))))

  :end-section-1)

(comment
  :start-section-2

                                        ; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
  (let [degs (fn [at-i m]
               (case m
                 1 (at-i [0 2 0 (at-i [2 3]) (at-i [7 4]) (at-i [4 5]) 3])
                 2 (at-i [(at-i [0  5 5])
                          2
                          (at-i [1 -7])
                          (at-i [8 2 8])
                          (at-i [4 7 3])
                          (at-i [6 5])
                          (at-i [3  8 9])])))
        bm 2
        bprob 0
        mm 2
        mprob 0.6
        mmix 0.3
        m1t -1
        m2t 3]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (degs at-i 2)]
                  (synth
                   :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                   :mod-freq (rrand 6000 10000)
                   :pan (mempan (mod deg 2))
                   :amp (rand-nth [(rrange 0.6 0.8)
                                   (rrange 0.4 0.7)])
                   :out (bh 0))

                  (when (or #_true (#{1 3} (mod (inc index) 5)))
                    (synth
                     :freq (deg->freq :base-freq 200 :scale 2 :degree deg)
                     :mod-freq (rrand 6000 10000)
                     :amp (rand-nth [(rrange 0.6 0.8)
                                     (rrange 0.4 0.7)])
                     :out (bh 0)))

                    ;; NOTE very cool new idea
                  (when (> (rand) 0.95)
                    (sub-rain {:durs [1 1 1] #_[3 2 2]
                               :ratio (at-i [1/9 2/9 3/9])
                               :on-event (let [degs (weighted {[1 2 3] 20
                                                               [3 2 1] 10
                                                               [3 5 8] 5})
                                               pans (sort (rand-nth [> <]) (map (fn [_] (rrand -1 1)) (range 4)))]

                                           (on-event (synth
                                                      :freq (deg->freq :base-freq 200 :scale 2 :degree (+ 4 deg (at-i degs)))
                                                      :mod-freq (rrand 6000 10000)
                                                      :pan (at-i pans)
                                                      :atk (weighted {0.01 10, 0.3 4 ,2 1})
                                                      :dcy (weighted {0.3 4, 2 2, 0.05 1,#_#_2 10})
                                                      :amp (rrange 0.5 0.8)
                                                      :out (bh 0))))})))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (degs at-i bm)]
                  (when (> bprob (rand))
                    (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [#_4 -4 3 -4 -3]) deg))
                               :dur (weighted {1 9 1.5 8})
                               :vel 100})))))
    (ref-rain
     :id ::1-m :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (+ (degs at-i mm)
                             (at-i [2 2 3 2 3])
                             (weighted {0 5 4 4 8 3}))]
                  (when (> mprob (rand))
                    (when (> (rand) mmix)
                      (my-malgo {:deg (diat->polydori-degree 0 (+ m1t  deg))
                                 :dur (weighted {0.1 9 1 5})
                                 :vel (at-i [80 100 80 60 77])}))
                    (when (> (rand) mmix)
                      (my-malgo {:deg (diat->polydori-degree 2 (+ m2t deg))
                                 :dur (weighted {0.1 9 1 5})
                                 :vel (at-i [80 100 80 60 77])})))))))

  :end-section-2)
