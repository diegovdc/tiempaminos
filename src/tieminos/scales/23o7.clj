(ns tieminos.scales.23o7
  (:require
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.osc.surge :as surge]
   [time-time.dynacan.players.gen-poly :as gp]))

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
  (let [scale (dedupe-scale (ratios->scale 2
                                           (let [cell [1 23/7 4/3]]
                                             (concat cell
                                                     (map #(* 4/3 %) cell)
                                                     (map #(* 3/2 %) cell)))))
        scale-data {:meta {:scl/name "23/7"
                           :scl/description "Experiment"}
                    :scale scale}
        scale-size (count (:scale scale-data))]
    (gp/ref-rain
      :id :sequencer2
      :tempo 80
      :durs [1/2]
      :on-event (gp/on-event
                  #_(malgo-note {:sink sink
                               :dur 1
                               :vel (int (* 1.1 (at-i [100 80 90])))
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg (+ (rand-nth [7 0 -7 14]) (at-i [0 1 3 5 6]))})
                  (when (#{} (mod i 3))
                    (malgo-note {:sink sink
                                 :dur 1/4
                                 :vel (at-i [127 100 90])
                                 :scale-size scale-size
                                 :base-midi-deg 60
                                 :deg (+ -7 (at-i [3 (at-i [1 4])
                                                   (at-i [5 3 5])
                                                   (at-i [4 5 6])]))}))
                  #_(when (#{0 1 2} (mod i 3))
                    (malgo-note {:sink sink
                                 :dur 1/4
                                 :vel (at-i [127 100 90])
                                 :scale-size scale-size
                                 :base-midi-deg 60
                                 :deg (+ 7 (at-i [3 (at-i [1 4])
                                                  (at-i [5 3 5])
                                                  (at-i [4 5 6])]))}))
                  #_(when (#{0 1 3 2 5 6} (mod i 7))
                    (malgo-note {:sink sink
                                 :dur 3
                                 :vel (at-i [127 100])
                                 :scale-size scale-size
                                 :base-midi-deg 60
                                 :deg (+ (rand-nth [ 14 14 28]) (at-i [(at-i [9 8 7]) 6 5 4 3]))}))

                  #_(when (#{0 2} (mod i 2))
                    (malgo-note {:sink sink
                                 :dur 1
                                 :vel (at-i [127])
                                 :scale-size scale-size
                                 :base-midi-deg 60
                                 :deg (+ 28 (at-i [0 3 0 0 1]))}))
                  (when (#{0 1} (mod i 2))
                    (malgo-note {:sink sink
                                 :dur 5
                                 :vel (at-i [127])
                                 :scale-size scale-size
                                 :base-midi-deg 60
                                 :deg (+ -14 (at-i [0 3 5 0 1]))}))))
    (gp/ref-rain
      :id :sequencer4
      :tempo 80
      :durs [1/4]
      :on-event (gp/on-event
                  (malgo-note {:sink (at-i [sink sink2])
                               :dur 1
                               :vel (int (* 0.7 (at-i [100 80 90])))
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg (+ (rand-nth [-7]) (at-i [0 1]))})
                  (malgo-note {:sink (at-i [sink sink2])
                               :dur 1
                               :vel (min 127 (int (* 0.7 (at-i [100 80 90 127]))))
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg (+ (at-i [14 21]) (at-i [0 -7 0 -14]))})))
    (gp/ref-rain
      :id :sequencer3
      :tempo 80
      :ref :sequencer2
      :durs [1/3 1/3 1/2]
      :on-event (gp/on-event
                  (when (#{0 3 4 7} (mod i 5))
                    #_(malgo-note {:sink sink2
                                 :dur 1/4
                                 :vel (+ 40 (at-i [20 10 30 20 50]))
                                 :scale-size scale-size
                                 :base-midi-deg 60
                                 :deg (+ 14 (at-i [0 3 5 13 8 -9 7]))}))))
    #_(gp/stop :sequencer3)
    #_(gp/stop)
    (surge/set-scale
      {:scale scale-data
       :scale-name "23o7[7]-NEJI"})
    scale-data)
  (gp/stop))
