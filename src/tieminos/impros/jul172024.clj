(ns tieminos.impros.jul172024
  "Recorded on the `july-7th-harmonic-month` reaper project.
  Uses a scale from `tieminos.scales.17o7`"
  (:require
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.osc.surge :as surge]
   [tieminos.polydori.scale :refer [dedupe-scale]]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))
  (surge/init)
  (gp/stop :sequencer2)
  (let [scale (dedupe-scale (ratios->scale 3
                                           (let [cell [1 17/7 4/3]]
                                             ;; chida
                                             (concat cell
                                                     (map #(* 3/2 %) cell)
                                                     (map #(* 3/2 4/3 %) cell)))))
        scale-data {:meta {:scl/name "17/7"
                           :scl/description "An experiment"}
                    :scale scale}
        scale-size (count (:scale scale-data))]
    (gp/ref-rain
     :id :sequencer2
     :tempo 160
     :durs [1/2]
     :on-event (gp/on-event
                #_(println data)
                (when (#{0 1 3} (mod i 4))
                  (malgo-note {:sink sink
                               :dur 1/30
                               :vel (min 127 (int (* 12 (at-i [8 3 4 5 3]))))
                               :scale-size scale-size
                               :base-midi-chan 2
                               :base-midi-deg 60
                               :deg (+ (at-i [4 4 4 1 4 1 1 1 4 1 1])
                                       (at-i (range (count scale))))}))

                (when (#{1 2} (mod i 3))
                  (malgo-note {:sink sink2
                               :dur 1/3
                               :vel (at-i [127 80 80])
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg 0}))
                (when (#{1 2 4 6 8 9} (mod i 11))
                  (malgo-note {:sink sink3
                               :dur 1/3
                               :vel (at-i [127 80 80])
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg (at-i [0 0 5 6 7])}))))
    (surge/set-scale
     {:scale scale-data
      :scale-name "dev/17o7-"})
    scale-data)
  (gp/stop))
