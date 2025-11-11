(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.uterofonia.main
  (:require
   [overtone.core :as o]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.init :as bardo.init]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc :as bardo.osc]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]))

(comment
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NOTE main initialization section
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; init OSC communication
  (bardo.osc/init!
    ;; NOTE if an client is missing there will be a "Host is Down" error.
   [["127.0.0.1" 16181]       ;; local
    #_["192.168.0.101" 16180] ;; diego
    #_["192.168.0.104" 16180] ;; milo
    ])
  (bardo.osc/reset-default-state!)
  ;; init everything (habitat and input synths) except SC, REAPER and OSC communications
  (bardo.init/all!)
  (bardo.osc/post-live-state-to-ui!)
  (bardo.osc/post-live-state-to-ui! :print-instead? true))

(comment
  (o/stop)
  (def out (+ 19 34))
  (def ute (voice-synth {:interface-in 3
                         :out out
                         :rroom 0.8
                         :rdamp 1
                         :rmix 1
                         :pan-rate 0.2
                         :width-lfo-freq 0.1
                         :min-width 2.5
                         :max-width 5
                         :amp 2}))
  (o/ctl ute
         :rroom 0.7
         :rdamp 1
         :rmix 0.8
         :pan-rate 0.2
         :width-lfo-freq 0.1
         :min-width 2.5
         :max-width 5
         :amp 2)
  (def maricarmen (voice-synth {:interface-in 4
                                :out out
                                :rroom 0.8
                                :rdamp 1
                                :rmix 1
                                :pan-rate 0.2
                                :width-lfo-freq 0.1
                                :min-width 2.5
                                :max-width 5
                                :amp 2}))
  (o/ctl maricarmen
         :rroom 0.7
         :rdamp 1
         :rmix 0.8
         :pan-rate 0.2
         :width-lfo-freq 0.1
         :min-width 2.5
         :max-width 5
         :amp 2)

  (def yesica (voice-synth {:interface-in 5
                            :out out
                            :rroom 0.8
                            :rdamp 1
                            :rmix 1
                            :pan-rate 0.2
                            :width-lfo-freq 0.1
                            :min-width 2.5
                            :max-width 5
                            :amp 2}))

  (o/ctl yesica
         :rroom 0.7
         :rdamp 1
         :rmix 0.8
         :pan-rate 0.2
         :width-lfo-freq 0.1
         :min-width 2.5
         :max-width 5
         :amp 2))

(oe/defsynth voice-synth
  [interface-in 0
   pan-rate 0.2
   orientation 0.5
   width-lfo-freq 0.1
   min-width 2.5
   max-width 4
   a 5
   release 10
   gate 1
   rmix 0.1
   rroom 0.8
   rdamp 0.8
   amp 1
   out 0]
  (o/out out
         (let [sig (-> (o/sound-in interface-in)
                       (#(oe/circle-az :num-channels 4
                                       :in %
                                       :pos (o/lf-noise1 pan-rate)
                                       :width (lfo-kr width-lfo-freq min-width max-width)
                                       :orientation orientation)))
               sig-rev* (o/free-verb sig 1 (* (o/rand 0.9 1) rroom) rdamp)
               sig-rev (* 1/4 (apply +
                                     (map (fn [d] (* rmix
                                                     (o/rand 0.9 1)
                                                     (o/delay-l  sig-rev* d d)))
                                          [0.01 0.009 0.005 0.003])))]

           (* (+ sig sig-rev)
              amp
              (o/env-gen (o/env-adsr a 1 1 release :curve -0.5)
                         gate
                         :action o/FREE)))))
