(ns tieminos.habitat.synths.granular
  (:require [overtone.core :as o]
            [tieminos.overtone-extensions :as oe]
            [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(defn rand-start-end
  []
  (let [start (rand 0.9)
        end (+ start (rand (- 1 start)))]
    {:start start :end end}))

(comment
  (require '[tieminos.sc-utils.recording.v1 :refer [bufs]]
           '[tieminos.compositions.garden-earth.synths.granular :as gs]
           '[tieminos.habitat.recording :as rec])

  (o/defsynth play-buf*
    [buf 0]
    (o/out 0 (o/play-buf 1 buf)))

  ;; todo generate bufs go to `tieminos.sc-utils.recording.v1`
  ;; and play some stuff in the `comment` at the bottom
  (->> @rec/bufs keys)
  (oe/defsynth amanecer*guitar-clouds
    ;; TODO pass in manual envelope
    [buf 0
     trig-rate 40
     grain-dur 1/20
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
     out 0
     lpf-min 100
     lpf-max 2000
     pan 0
     rev-mix 1
     rev-room 0.5
     a-level 1]
    (o/out out
           (-> (o/grain-buf
                :num-channels 1
                :trigger (o/impulse trig-rate)
                :dur grain-dur
                :sndbuf buf
                :rate rate
                :pos  (o/line start end (+ a d r))
                :pan 0)
               #_(o/lpf (lfo 0.1 lpf-min lpf-max))
               (#(o/pan-az 4 % :pos pan :width (lfo 0.1 1 2.5)))
               (o/free-verb rev-mix rev-room)
               (* amp
                  #_(lfo 0.1 0 1)
                  (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                         [-1 -5])
                             :action o/FREE)))))
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

