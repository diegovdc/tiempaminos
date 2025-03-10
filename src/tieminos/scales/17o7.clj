(ns tieminos.scales.17o7
  (:require
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.osc.surge :as surge]
   [time-time.dynacan.players.gen-poly :as gp]))

;;  some scales with 17 and 7
(def scales-17&7
  (map #(ratios->scale 3 %)
       [(let [cell [1 17/7]]
          ;; chida
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 17/5 3/2 %) cell)))

        (let [cell [1 17/16 (* 3 7/17) 4/3]]
          ;; no ta mal
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 3/2 3/2 %) cell)))
        ;; bonita
        [1
         2
         (* 2 17/5)
         (* 2 17/5 17/5)
         4/3
         3/2
         17/5
         (* 3/2 17/5)]
        [1
         17/11
         17/7
         7/4
         (* 8/7 7/4)
         2
         3/2
         4/3
         17/5
         17/9
         (* 17/9 10/9)
         10/6]
        (let [cell [1 17/7 4/3]]
          ;; chida
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 3/2 4/3 %) cell)))]))

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
      :scale-name "dev/17o7"})
    scale-data)
  (gp/stop))

(comment
  (require '[tieminos.lattice.v1.lattice :as lattice.v1])

  (lattice.v1/draw-lattice
   {:ratios (let [cell [1 17/7 4/3]]
              (concat cell
                      (map #(* 3/2 %) cell)
                      (map #(* 3/2 4/3 %) cell)))
    :period 3
    :custom-edges #{17/7}
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])})

  ;; analysis of

  (->> (let [cell [1 17/7 4/3]]
         (concat cell
                 (map #(* 3/2 %) cell)
                 (map #(* 3/2 4/3 %) cell)))
       (ratios->scale 3)
       (map :ratio)
       sort
       (dedupe)))

(comment

  ;; as two pentatonic scales a fifth appart

  (map #(* % 2/3) [#_3/2 34/21 2/1 17/7 8/3 3/1]) ;; (68/63 4/3 34/21 16/9 2N)

  [17/14 4/3 3/2 34/21 2]
  [68/63 4/3 34/21 16/9 2N]

  (map :ratio (dedupe-scale (ratios->scale (concat
                                            [17/14 4/3 3/2 34/21 2]
                                            (map #(/ % 16/9) [68/63 4/3 34/21 16/9 2N])))))
  (ratios->scale [17/14 4/3 3/2 34/21 2])
  (surge/set-scale
   {:scale {:meta {:scl/name "second child of 17/7"
                   :scl/description "An experiment"}
            :scale  (dedupe-scale (ratios->scale (concat
                                                  [17/14 4/3 3/2 34/21 2]
                                                  (map #(/ % 16/9) [68/63 4/3 34/21 16/9 2N]))))}
    :scale-name "dev/17o7-penta1"}))
