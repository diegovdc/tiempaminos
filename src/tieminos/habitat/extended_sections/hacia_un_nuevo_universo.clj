(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo
  (:require
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

;; IDEAS
;; 1. resonant-reverberant filters with a bit of delay that come for another place vs the source sound
;; 2. explore occupying the whole of the space - only needs to use width from 1-4, but 4 is not perfectly balanced, though it sort og works
;; 3. explorar pitch-shift
;; (
;; // use PitchShift to granulate input - USE HEADPHONES to prevent feedback.
;; // upper left corner is normal playback. x = pitch dispersion, y = time dispersion
;; var grainSize;
;; grainSize = 0.1;
;; play({
;; 	[SoundIn.ar([0, 1]),
;;     PitchShift.ar(
;;         SoundIn.ar([0, 1]),
;;         grainSize,
;;         MouseX.kr(1,3/2),                        // nominal pitch rate = 1
;;         0,             // pitch dispersion
;;         MouseY.kr(0, grainSize)    // time dispersion
;; 	)]
;; }))
;; ;;
;;
;; 4. explorar diferentes armonía

(oe/defsynth basic-pitch-shifter
    [in 0
     ratio 1
     amp 1
     gate 1
     out 0]
    (o/out out
           (-> (o/pitch-shift
                 (o/in in)
                 0.1
                 ratio
                 0
                 0)
               (#(oe/circle-az {:num-channels 4
                                :in %
                                :pos (lfo 0.1 -1 1)}))
               (* amp (o/env-gen (o/env-asr 5 1 5)
                                 :gate gate
                                 :action o/FREE)))))

(oe/defsynth algo-basic-pitch-shifter
  [in 0
   ratio 1
   amp 1
   a 5
   r 5
   dur 1
   out 0]
  (o/out out
         (-> (o/pitch-shift
               (o/in in)
               0.1
               ratio
               0
               0)
             (#(oe/circle-az {:num-channels 4
                              :in %
                              :pos (lfo 0.1 -1 1)}))
             (* amp (o/env-gen (o/envelope [0 1 1 0]
                                           [a dur r])
                               :action o/FREE)))))


(comment
  (o/defsynth oli
    []
    (o/out 0
           (o/pan-az 4
                     (-> (o/saw 200)
                         (o/lpf 800))
                     (o/mouse-x:kr -1 1)
                     1
                     (o/mouse-y:kr 1 4))))



  (def bps (basic-pitch-shifter {:group (groups/mid)
                                 :in (-> @inputs :mic-1 :bus)
                                 :ratio 19/16 #_(+ 3/2 1/10)
                                 :out 0}))
  (def bps2 (basic-pitch-shifter {:group (groups/mid)
                                  :in (-> @inputs :mic-1 :bus)
                                  :ratio (+ 3/2 )
                                  :out 0}))
  (def bps3 (basic-pitch-shifter {:group (groups/mid)
                                  :in (-> @inputs :mic-1 :bus)
                                  :ratio (+ 8/7 )
                                  :out 0}))

  (o/ctl bps :gate 0)
  (o/ctl bps2 :gate 0)
  (o/ctl bps3 :gate 0)

  (o/kill bps)
  (oli)

  (o/stop)

  (o/demo 20
          (-> (o/pitch-shift
                (o/in (-> @inputs :mic-1 :bus))
                0.2
                ;; using chords like this produce some interesting sonorities
                [(+ 3/2 0.1)
                 #_(+ 3/2 0.12)
                 ;; and higher pitched ratios produce interesting artifacts
                 #_(+ 13/8)]
                0
                ;; keep exploring this
                (o/mouse-y:kr 0 0.2))
              #_(#(oe/circle-az {:num-channels 4
                                 :in %
                                 :pos (lfo 0.1 -1 1)})))))
