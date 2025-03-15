(ns tieminos.scales.7&31
  (:require
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.osc.surge :as surge]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))
  (surge/init)
  (gp/stop :sequencer2)
  (let [scale (dedupe-scale (ratios->scale 2
                                           #_(let [cell [1 29/7 35/29 4/3]]
                                               (concat cell
                                                       (map #(* 3/2 %) cell)))
                                           ;; "dev/31o28[5]"
                                           #_(let [cell [1 31/28
                                                         (* 3/2 31/28)
                                                         (* 7/6 31/28)
                                                         (* 7/6 31/28 31/28)]]
                                               cell)
                                           #_(let [cell [1 31/28
                                                         (* 3/2 31/28)
                                                         (* 7/6 31/28)
                                                         (* 7/6 31/28 31/28)]]
                                               (concat cell
                                                       (map (fn [r] (* 4/3  r)) cell)))
                                           ;; "dev/31o28[7]"
                                           (let [cell [1 31/28
                                                       (* 31/28 31/28)
                                                       (* 3/2 31/28)
                                                       (* 3/2  31/28 31/28)
                                                       (* 7/6 31/28)
                                                       (* 7/6 31/28 31/28)]]
                                             (concat cell
                                                     #_(map (fn [r] (* 4/3  r)) cell)))))
        scale-data {:meta {:scl/name (str "dev/31o28[7]")
                           :scl/description "An experiment"}
                    :scale scale}
        scale-size (count (:scale scale-data))]
    #_(gp/ref-rain
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
      :scale-name  (:scl/name (:meta scale-data))})
    scale-data)
  (gp/stop))

(comment
  ;; anaysis
  (erv.utils.conversions/ratio->cents 31/28)
  (erv.utils.conversions/ratio->cents 13/12)
  (erv.utils.conversions/ratio->cents 12/11)
  (erv.utils.conversions/ratio->cents 11/10)
  (erv.utils.conversions/ratio->cents 19/17)
  (erv.utils.conversions/ratio->cents 11/9)
  (erv.utils.conversions/ratio->cents 32/28)
  (erv.utils.conversions/ratio->cents 30/28)

  (require '[tieminos.lattice.v1.lattice :as lattice.v1])

  (lattice.v1/draw-lattice
   {:ratios    (map (fn [r] (/ r 31/28))
                    (let [cell [1 31/28
                                (* 31/28 31/28)
                                (* 3/2 31/28)
                                (* 3/2  31/28 31/28)
                                (* 7/6 31/28)
                                (* 7/6 31/28 31/28)]]
                      cell))
    :period 2
    :custom-edges #{31/28 7/6}
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[31 19]])}))
