(ns tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-10-06-2025
  (:require
   [erv.scale.core :as scale]
   [tieminos.compositions.7d-percusion-ensamble.base
    :as *7d-base
    :refer [root]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-space
    :as space]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.utils
    :refer [subrain]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.wavetable
    :refer [akwf-samples available-dirs]]
   [tieminos.math.bezier-samples :as bezier-a]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.seq-utils.core
    :refer [** ++ choose graph lin mancha mirror mseq rainseq ret rev xo]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.synths.mono :as ms :refer [bd]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(defn get-subscps-scale
  [cps-name]
  (:scale (get-in polydori-v2 [:subcps cps-name])))

(def diat-neighbors
  {"14b" "2)4 of 4)7 3.19-7.9.15.21"
   "12b" "2)4 of 4)7 15.19-3.7.9.21"
   "10b" "2)4 of 4)7 1.19-3.7.9.21"})

(defn *7d-deg->freq
  [root scale-id degree]
  (if (number? scale-id)
    (:freq (*7d-base/deg->data
            :base-freq root
            :scale scale-id
            :degree degree))
    (scale/deg->freq (get-subscps-scale
                      (get diat-neighbors scale-id "2)4 of 4)7 1.19-3.7.9.21"))
                     root
                     degree)))
(comment
  (*7d-deg->freq 100 1 1))

(comment
  (available-dirs)
  (def akwf (apply akwf-samples ["sinharm" "blended" "bw_sin"]))
  (ms/mono-mooglad
   (merge (akwf :bw-sin)
          {:amp 4 :freq 560 :out 8}))

  (def waves [:sinharm :blended :bw-sin])
  (def synths [ms/mooglad ms/low ms/bd ms/hh ms/hh ms/metalstr ms/noistr ms/sharp-plate ms/snare]))

(comment
  (concat (bezier-a/fsf 24 0.3 3)
          (reverse (bezier-a/fsf 24 0.3 4)))
  (repcat [1 true]
          [23 false]))

(comment

  (rain.v2/ref-rain
   :id :one
   :durs [2 3 2 2 3 2 3 3]
   :ratio 1/4
   :on-event (rain.v2/on-event
              #_(bd {:freq (*7d-deg->freq (* root (rainseq {1/2 4 1 1 2 1})) 13
                                          (rainseq {-6 5 -4 2}))
                     :d 3
                     :amp (rrange 0.1 0.4)
                     :out (rainseq [0 7 14 22])})
              #_(when (xo "xoooooo " i)
                  (subrain {:ref :one
                            :durs [1/2 1/2 1/4 1/2 1 1/4 1/4]
                            :delay 1/2
                            :on-event (rain.v2/on-event
                                       (ms/hh {:amp (rrange 0.5 1)
                                               :ff-gain (rrange 0.1 0.4)
                                               :d (rainseq {0.1 5 1 0.6})
                                               :out (rainseq [(graph space/left-wall)
                                                              (graph space/main-graph)
                                                              (graph space/right-wall)])}))}))))
  (rain.v2/ref-rain
   :id :two
   :ref :one
   :durs (concat (bezier-a/f 24 0.3 2)
                 (reverse (bezier-a/f 24 0.3 2))
                 (bezier-a/f 24 0.3 1)
                 (reverse (bezier-a/f 24 0.1 1)))
   :ratio 1/2
   :on-event (rain.v2/on-event
              (let [scale (rainseq (repcat [20 14]
                                           [20 13]))]
                #_(when (xo "xoooooxo " i)
                    (subrain {:ref :two
                              :durs (take (inc (rand 4)) [1/2 1/2 1/4 1/2 1 1/4 1/4])
                              :delay 1/2
                              :on-event (rain.v2/on-event
                                         (ms/metalstr
                                          {:freq (*7d-deg->freq (* root
                                                                   (rainseq [1 1 1 1 2 1 1 2]))

                                                                scale
                                                                (rainseq [1 [0 3 5] 5 [9 6 3] 7]))
                                           :out (rainseq (graph space/right-wall))}))}))
                #_(when (xo "xooooooooooo " i)
                    (subrain {:ref :two
                              :durs (take (inc (rand 4)) [1/2 1/2 1/4 1/2 1 1/4 1/4])
                              :delay 1/2
                              :on-event (rain.v2/on-event
                                         (ms/metalstr
                                          {:a 0.5
                                           :freq (*7d-deg->freq (* root
                                                                   (rainseq [1 1 1 1 2 1 1 2]))

                                                                scale
                                                                (rainseq [1 [0 3 5] 5 [9 6 3] 7]))
                                           :out (rainseq (graph space/right-wall))}))}))
                #_((rand-nth [ms/noistr ms/metalstr])
                   {:freq (*7d-deg->freq (* root (rainseq (repcat [10 1/2]
                                                                  [8 2]
                                                                  [10 1])))
                                         scale
                                         (rainseq (repcat [5 0]
                                                          [10 (lin 1 7 3 6 4 5 8)]
                                                          [4 1]

                                                          [3 2]
                                                          [3 (lin 1 7 3 6 4 5 8)]
                                                          [10 3])))
                    :d (rainseq (range 0.4 2 0.1))
                    :out (rainseq space/spiral-left-up-seq)})))))
