(ns tieminos.compositions.garden-earth.fl-grain-1.dot-waves
  (:require
   [tieminos.compositions.garden-earth.base
    :refer [early-g
            get-pc
            interval-from-note
            on-event
            ref-rain
            scale->pr-fingerings
            stop
            subcps]]
   [tieminos.compositions.garden-earth.synths.granular :as granular]
   [tieminos.compositions.garden-earth.synths.recording :as rec]))

(do
  ;; TODO fix test-samples usage
  #_(defn ex1 []
      (let [bufs @rec/test-samples
            scale (subcps "1)4 of 3)6 1.11-3.5.7.9")
            scale2 (subcps "3)5 of 3)6 1.5.7.9.11")]
        (println (scale->pr-fingerings scale2)
                 "\n\n")
        (ref-rain
         :id ::ex1
         :tempo 120
         :ratio 1/10
         :durs [5 3 5 3 3]
         :on-event (on-event
                    (let [note (rand-nth scale)
                          target (- (rand-int 29) 29)
                          pc (get-pc note)
                          b (rec/get-any-buffer-for-pitch-class pc bufs)
                          rate (interval-from-note scale2 note target)]
                      #_(println (pitch-class->pr-fingering pc)
                                 "\n\n")
                      (vec
                       (for [g-dur (subvec (shuffle [1/10 1/8 1/12 1/20 1/2]) 0 2)
                             dur* (subvec (shuffle [0.3 0.2 0.1 1 2]) 0 2)]
                         (granular/dot
                          {:group (early-g)
                           :out 8
                           :buf b
                           :dur dur*
                           :trig-rate 10
                           :grain-dur g-dur
                           :rate rate
                           :speed 0.1
                           :amp 10
                           :min-amp 0.8
                           :amp-lfo 0.01
                           :start 0 #_start
                           :end 1 #_ end
                           :mix 1
                           :room 3
                           :damp 0.8
                           :pos-noise-freq 1000
                           :pos-noise-amp 0.2
                           :a dur*
                           :r 0.1}))))))))
  (comment (ex1)))

(comment
  (stop)
  (do
    ;; run reverbs on out 8-9
    ;; stereo capture of the signal
    (rev [:tail fx-g] 8 :amp 2)
    (rev [:tail fx-g] 9 1 :amp 2)))
