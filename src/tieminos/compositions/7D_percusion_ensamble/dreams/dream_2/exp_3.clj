(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.exp-3
  "Chords"
  (:require
   [tieminos.compositions.7D-percusion-ensamble.base
    :as *7d-base
    :refer [root]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-space
    :refer [main-graph]]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.utils
    :refer [subrain]]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.wavetable
    :refer [akwf-samples available-dirs mono-mooglad]]
   [tieminos.seq-utils.core :refer [++ graph lin mseq ret xo]]
   [tieminos.seq-utils.utils :refer [subgraph]]
   [time-time.dynacan.players.refrain.v2
    :as
    rain.v2
    :refer
    [on-event ref-rain]]))

(comment
  (def akwf (apply akwf-samples (take 3 (available-dirs))))

  (rain.v2/stop)
  (ref-rain
   :id :exp-3/chords
   :durs [1/4]
   :on-event (on-event
              (let [[chord out] (mseq i [(ret [0 2 4] 0)
                                         [(ret [0 2 4] 0)
                                          (ret [6 2 4] 0)]
                                         (ret [3] 6)
                                         (ret [0 (mseq i (lin :id/a 1 2 4 5 6 7)) 4] 0)
                                         (ret [-1 1 3] 1)
                                         [(ret [-1 (mseq i (lin :id/a 1 2 4 5 6 7)) 3] 1)
                                          (ret [5 7 9] 1)]])

                    scale (mseq i (concat (repeat 15 0)
                                          (repeat 20 11)
                                          #_(repeat 20 13)
                                          #_(repeat 30 11)
                                          (repeat 10 [9 6])
                                          (repeat 15 0)
                                          (repeat 5 9)
                                          (repeat 15 6)))
                    degs1 (++ (apply lin (concat (repeat 10 [-4 2])
                                                 (repeat 4 [-5 -5 1])
                                                 (repeat 10 [0 0 3])
                                                 (repeat 4 [2 5])
                                                 (repeat 10 [-3 3])))
                              (apply lin [1 2 [3 4] [5 6 7] 8 9]))]

                (doseq [[i deg] (map-indexed vector chord)]
                  (let [freq (:freq (*7d-base/deg->data
                                     :base-freq root
                                     :scale scale
                                     :degree deg))
                        wave (mseq i {:sinharm 4
                                      :blended 4
                                      :bw-sin 2})]
                    (mono-mooglad (merge (akwf wave)
                                         {:freq (/ freq (rand-nth [4 1]))
                                          :range (mseq i {0.1 5 0.2 2 0.02 1 0.01 1})
                                          :a 0
                                          :s 0.2
                                          :r (mseq i {0.4 7 1 1 10 0.5})
                                          :amp (* 0 (mseq index [0.5 0.6 0.7 0.8]))
                                          :out (mod (+ out (* 2 i))
                                                    24)}))
                    #_(mono-mooglad (merge (akwf wave)
                                           {:freq (* 1 freq)
                                            :range (mseq i {0.1 5 0.2 2 0.02 1 0.01 1})
                                            :a 0.3
                                            :s 0.2
                                            :r (mseq i {0.4 7 1 1 10 0.5})
                                            :amp (* 0.05 (mseq index [0.5 0.6 0.7 0.8]))
                                            :out (mseq i (graph main-graph))}))))

                (when (xo "xoo" i)
                  (let [deg (mseq i degs1)
                        freq (:freq (*7d-base/deg->data
                                     :base-freq root
                                     :scale scale
                                     :degree deg))
                        octave (mseq i [1 1 1/2 1 1 1 2 2 1/2 1/2 4 3])]
                    (subrain
                     {:ref :exp-3/chords
                      :ratio (mseq i (concat (repeat 1 1/2)
                                             (repeat 4 2/3)))
                      :durs (repeat 4 1/4)
                      :on-event (on-event
                                 (mono-mooglad (merge (akwf :blended)
                                                      {:freq (* octave freq)
                                                       :range (mseq i {0.1 5 0.2 2 0.02 1 0.01 1})
                                                       :a 0.3
                                                       :s 0.4
                                                       :r (mseq i {0.4 7 1 1 10 0.5})
                                                       :amp (* 0.0 (mseq index [0.5 0.6 0.7 0.8]))
                                                       :out (mseq i (graph main-graph))})))})))

                (when (xo "xx" i)
                  (let [deg (mseq i degs1)
                        freq (:freq (*7d-base/deg->data
                                     :base-freq root
                                     :scale scale
                                     :degree deg))
                        harmonics [1 1 1 1/2 1/3 1/4]
                        harmonic (mseq i harmonics)]
                    (subrain
                     {:ref :exp-3/chords
                      :ratio (mseq i [1/10 1/11 1/12 1/13])
                      :durs (repeat (+ 24) 1/4)
                      :on-event (on-event
                                 (mono-mooglad (merge (akwf :blended)
                                                      {:freq (* freq harmonic 1/48)
                                                       :range (mseq i {0.1 5 0.2 2 0.02 1 0.01 1})
                                                       :a 0
                                                       :s-amp 1
                                                       :s 0.1
                                                       :r (mseq i {0.1 7})
                                                       :amp (* 0. (mseq index (range 0.1 1 1/24)))
                                                       :out (mseq i (graph main-graph))})))})))))))
