(ns tieminos.scales.14o7-5ed2-neji
  (:require
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.osc.surge :as surge]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]))

(def symmetric-5t-7&13
  {:meta {:scl/name "7&13-5ED2-NEJI"
          :scl/description "Symmetric NEJI based on 14/13^2"}
   :scale (ratios->scale [1
                          (* 14/13 14/13)
                          #_(* 14/13 14/13 8/9)
                          (* 13/7 13/14)
                          4/3
                          3/2
                          #_(* 3/2 9/8)])})
(comment
  (surge/set-scale
    {:scale symmetric-5t-7&13
     :scale-name (:scl/name (:meta symmetric-5t-7&13))}))



(comment
  ;; Live tuning experiment
  (def sink (midi/midi-out "VirMIDI"))
  (surge/init)
  (gp/stop :sequencer2)
  (let [cell [1 15/7 5/3 4/3]
        scale {:meta {:scl/name "Live tuning"
                      :scl/description "An experiment"}
               :scale (ratios->scale (concat cell
                                             (map #(* 15/7 %) cell)
                                             (map #(* 4/3 5/3 %) cell)
                                             (map #(* 3/2 15/7  %) cell)))}
        scale-size (count (:scale scale))]
    (ref-rain
      :id :sequencer2
      :tempo 160
      :durs [3 2]
      :on-event (on-event
                  (malgo-note {:sink sink
                               :dur 1
                               :vel (int (* 1 (at-i [80 50 80 60 100])))
                               :chan 6
                               :scale-size scale-size
                               :base-midi-deg 60
                               :base-midi-chan 6
                               :deg (int (+ (rand-nth [0
                                                       (* -1 scale-size )
                                                       (* 1/2 scale-size )])
                                            0
                                            (* 2 (rand-nth [0 4 4.5 7 10]))))})))
    (surge/set-scale
      {:scale scale
       :scale-name "dev/16t-cluster-15o7-"})
    scale))
