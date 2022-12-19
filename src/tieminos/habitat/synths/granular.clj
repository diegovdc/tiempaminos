(ns tieminos.habitat.synths.granular
  (:require [overtone.core :as o]
            [tieminos.overtone-extensions :as oe]
            [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(comment
  (require '[tieminos.sc-utils.recording.v1 :refer [bufs]]
           '[tieminos.compositions.garden-earth.synths.granular :as gs])
  (do
    (o/defsynth play-buf*
      [buf 0]
      (o/out 0 (o/play-buf 1 buf)))

    ;; todo generate bufs go to `tieminos.sc-utils.recording.v1`
    ;; and play some stuff in the `comment` at the bottom
    (->> @bufs keys)
    (oe/defsynth amanecer*guitar-clouds
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
                  :trigger (o/impulse trig-rate)
                  :dur [grain-dur]
                  :sndbuf buf
                  :rate rate
                  :pos (+  #_(lfo 200 -0.02 0.02) (o/line start end (+ a d r)))
                  :pan 0)
                 (o/lpf (lfo 0.1 100 5000))
                 (#(o/pan-az 4 % :pos (lfo 0.1 -1 1) :width (lfo 0.1 1 2.5)))
                 (o/free-verb 1 2 0.1)
                 (* amp
                    (lfo 0.1 0 1)
                    (o/env-gen (o/envelope [0 1 d-level 0] [a d r]
                                           [-1 -5])
                               :action o/FREE)))))

    (amanecer*guitar-clouds
     {:buf (:mic-2-bus-1671203082901
            #_(rand-nth [:guitar-bus-1670702380203
                         :guitar-bus-1670702407392])
            @bufs) #_(:guitar-bus-1670702407392 @bufs)
       ;; ads combo `:a 0.1 :d 0.1(lvl ~0.3) :r 5+` works rather nice
      :a 0.1
      :d 0.1
      :d-level 0.2
      :r 10
      :rate (* (rand-nth [2 3 4 1]) (rand-nth [13/8 3 5/4 1 2 3/2 11/6 7/8 1/2]))
      :amp 40
      :start (rand)
      :end 1}))
  (play-buf* (->> @bufs :guitar-bus-1671203309129))
  (->> @bufs keys)
  (save-samples :test)
  (o/stop)
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus]])
  (defn rec-input [input-bus]
    (let [name* (:name input-bus)]
      (start-recording
       :bufs-atom bufs
       :buf-key (keyword (str name* "-" (o/now)))
       :input-bus input-bus
       :seconds 5
       :msg "Test recording"
       :on-end replay)))
  (rec-input guitar-bus)
  (rec-input mic-2-bus))
