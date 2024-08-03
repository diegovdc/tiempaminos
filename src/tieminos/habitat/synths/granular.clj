(ns tieminos.habitat.synths.granular
  (:require
   [overtone.core :as o]
   [tieminos.habitat.recording :refer [norm-amp]]
   [tieminos.habitat.recording :as rec]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo lfo-kr]]
   [tieminos.utils :refer [rrange]]))

(defn rand-start-end
  []
  (let [start (rand 0.9)
        end (+ start (rand (- 1 start)))]
    {:start start :end end}))

(oe/defsynth amanecer*guitar-clouds
    ;; TODO pass in manual envelope
  [buf 0
   trig-rate 40
   grain-dur 1/20
   rate 1
   amp 1
   amp-lfo-min 0.5
   amp-lfo 0.1
   start 0.1
   end 0.3
   a 0.1
   d 1
   d-level 0.3
   r 3
   out 0
   lpf-min 100
   lpf-max 2000
   pan 0
   rev-mix 1
   rev-room 0.5
   interp 1
   a-level 1]
  (o/out out
         (-> (o/grain-buf
              :num-channels 1
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos  (o/line start end (+ a d r))
              :interp interp
              :pan 0)
             (o/lpf (lfo 0.1 lpf-min lpf-max))
             (#(o/pan-az 4 % :pos pan :width (lfo 0.1 1 2.5)))
             (o/free-verb rev-mix rev-room)
             (* amp
                #_(lfo amp-lfo amp-lfo-min 1)
                (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                       [-1 -5])
                           :action o/FREE)))))

(comment

  (-> rec/bufs)
  (-> (:amanecer-ide-mic-1-bus-3 @rec/bufs)
      ;; keys
      :amp-norm-mult
      ;; :rec/meta
      )

  (do
    (oe/defsynth clouds2-4ch
      ;; TODO pass in manual envelope
      [buf 0
       trig-rate 40
       grain-dur 1/20
       rate 1
       amp 1
       amp-lfo-min 0.5
       amp-lfo 0.1
       start 0.1
       end 0.3
       a 0.1
       d 1
       d-level 0.3
       r 3
       out 0
       lpf-min 100
       lpf-max 2000
       pan 0
       rev-mix 1
       rev-room 0.5
       interp 1
       a-level 1
       root 200
       moog-freq 200
       moog-reso 0.5
       dly-time-mult 1
       dly-mix 1 ;; produces a string like sound
       ]
      (let [comb-mix* (min 1 (max 0 dly-mix))]
        (o/out out
               (-> [(o/grain-buf
                      :num-channels 1
                      :trigger (o/impulse trig-rate)
                      :dur grain-dur
                      :sndbuf buf
                      :rate rate
                      :pos (o/line start end (+ a d r))
                      :interp interp
                      :pan 0)
                    (o/grain-buf
                      :num-channels 1
                      :trigger (o/impulse (* 13/7 trig-rate))
                      :dur (* 13/7 grain-dur)
                      :sndbuf buf
                      :rate rate
                      :pos (o/line start end (+ a d r)) #_(lfo 1 start end)
                      :interp interp
                      :pan 0)
                    (o/grain-buf
                      :num-channels 1
                      :trigger (o/impulse (* 19/17 trig-rate))
                      :dur (* 19/17 grain-dur)
                      :sndbuf buf
                      :rate rate
                      :pos  (lfo-kr 1 start end)
                      :interp interp
                      :pan 0)]
                   o/mix
                   ((fn [sig]
                      (+ (* (- 1 comb-mix*) sig)
                         (* comb-mix*
                            (o/comb-n sig
                                      (* dly-time-mult rate (/ 1 root))
                                      (* dly-time-mult rate (/ 1 root))
                                      -2)))))
                   (o/moog-ladder moog-freq moog-reso)
                   (o/svf) ;; could parametrize this
                   (#(o/pan-az 4 % :pos (lfo-kr 0.1 -1 1) :width (lfo-kr 0.1 1 2.5)))
                   (o/free-verb rev-mix rev-room)
                   (* amp
                      #_(lfo amp-lfo amp-lfo-min 1)
                      (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                             [-1 -5])
                                 :action o/FREE))))))

    ;; TODO left here WIP
    ;; Sounds good on long sounds
    ;; And also with short sounds where `d` is short and `d-level` goes significantly down
    ;; Can be computationally expensive
    ;; `dly-time-mult` can add softness
    ;; `dly-mix` can add a stringy sound (karlplus-strong)
    ;; `moog-freq` is a lpf freq -  a higher freq can let through the stringy sound
    (let [buf (:amanecer-ide-mic-1-bus-3 @rec/bufs)
          root 200]
      (doseq [r (rand-nth [[1 11/9 11/8 3/2]
                           [4/3 11/7 11/5 11/6]])]
        (clouds2-4ch
          {:buf buf
           :trig-rate 10
           :grain-dur 1/10
           :amp (*  (:amp-norm-mult buf))
           :start 0.5
           :end 0.9
           :interp 3
           :rate r
           :a 0.01
           :d 0.5
           :r 3
           :a-level 1
           :d-level 0.2
           :dly-mix 2
           :dly-time-mult 2
           :root root
           :moog-freq (* (rand-nth [1 2 8]) r root)
           :moog-reso 1
           :out 22
           })))))

(oe/defsynth amanecer*snare-mist
  ;; TODO pass in manual envelope
  [buf 0
   trig-rate 100
   grain-dur 1/10
   rate 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0.1
   end 0.3
   a 0.1
   d 1
   d-level 0.3
   r 3
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 1
              :trigger (o/dust trig-rate)
              :dur [grain-dur]
              :sndbuf buf
              :rate rate
              :pos (+  #_(lfo 200 start end) (o/line start end (apply + [a d r])))
              :pan 0)
             (o/lpf (lfo 0.1 20 2700))
             (#(o/pan-az 4 % :pos (lfo 0.1 -1 1) :width (lfo 0.1 1 2.5)))
             (* (lfo 5 0 1))
             (o/free-verb 0.5 2 0.1)
             (* amp
                (o/env-gen (o/envelope [0 1 d-level 0] [a d r]
                                       [-1 -5])
                           :action o/FREE)))))

(comment
  (require '[tieminos.sc-utils.recording.v1 :refer [bufs]]
           '[tieminos.compositions.garden-earth.synths.granular :as gs]
           '[tieminos.habitat.recording :as rec])

  (o/defsynth play-buf*
    [buf 0]
    (o/out 0 (o/play-buf 1 buf :action o/FREE)))

  (play-buf*
   (->> @rec/bufs vals rand-nth))
  (o/stop)
  (let [d (rrange 1 4)
        buf (->> @rec/bufs vals rand-nth)
        config {:buf buf
                :d d
                :amp 0.05
                :rev-room 4
                :trig-rate 200
                :grain-dur 1/100
                :a 2
                :amp-lfo 20
                :start 0 :end 1}
        rates (take (rand-int 5) [1 3 5 7 9])]
    (doseq [r rates]
      (amanecer*guitar-clouds (assoc config
                                     :rate r
                                     :amp (* (rrange 0.9 1) (norm-amp buf))
                                     :pan (rrange -1 1)))))
  ;; todo generate bufs go to `tieminos.sc-utils.recording.v1`
  ;; and play some stuff in the `comment` at the bottom
  (->> @rec/bufs keys)

  (play-buf* (:amanecer-pt4-mic-3-4 @rec/bufs))
  (oe/defsynth amanecer*snare-mist
    ;; TODO pass in manual envelope
    [buf 0
     trig-rate 100
     grain-dur 1/10
     rate 1
     amp 1
     min-amp 0.5
     amp-lfo 0.1
     start 0.1
     end 0.3
     a 0.1
     d 1
     d-level 0.3
     r 3
     out 0]

    (o/out out
           (-> (o/grain-buf
                :num-channels 1
                :trigger (o/dust trig-rate)
                :dur [grain-dur]
                :sndbuf buf
                :rate rate
                :pos (+  #_(lfo 200 start end) (o/line start end (apply + [a d r])))
                :pan 0)
               (o/lpf (lfo 0.1 20 2700))
               (#(o/pan-az 4 % :pos (lfo 0.1 -1 1) :width (lfo 0.1 1 2.5)))
               (* (lfo 5 0 1))
               (o/free-verb 0.5 2 0.1)
               (* amp
                  (o/env-gen (o/envelope [0 1 d-level 0] [a d r]
                                         [-1 -5])
                             :action o/FREE)))))

  #_(amanecer*snare-mist
     (merge
      {:buf (:test-mist-mic-2-1 @rec/bufs)
         ;; ads combo `:a 0.1 :d 0.1(lvl ~0.3) :r 5+` works rather nice
       :a 15
       :d 30
       :rate (rand-nth [4 2 3/4 2/3 7/4])
       :d-level 0.2
       :r 3
       :amp 10
       :start 1
       :end 1}
      #_(rand-start-end)))
  (play-buf* (:amanecer-pt4-mic-3-4 @rec/bufs))
  (->> @rec/bufs keys)
  (save-samples :test)
  (o/stop)
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus]]
           '[tieminos.habitat.recording :as rec])

  (rec/rec-input guitar-bus)
  (rec/rec-input {:section "test"
                  :subsection "mist"
                  :input-name "mic-2"
                  :input-bus mic-2-bus
                  :dur-s 7}))
