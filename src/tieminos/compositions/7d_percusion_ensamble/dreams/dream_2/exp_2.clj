(ns tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.exp-2
  "Wavetables"
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-space
    :refer [main-graph]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.utils
    :refer [subrain]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.wavetable
    :refer [akwf-samples available-dirs mono-mooglad]]
   [tieminos.seq-utils.core :refer [** ++ graph lin mseq xo]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.refrain.v2
    :as
    rain.v2
    :refer
    [on-event ref-rain]]))

(comment
  (def akwf (apply akwf-samples (available-dirs)))
  (ref-rain
   :id :test
   :durs [1/4]
   :on-event (on-event
              (let [out1  (mseq i [16 10 0])
                    out2  (mseq i [13 1 18]) #_(mseq i (graph tri6-1))
                    mult1 (if (even? i) 1 1/2)
                    mult2 (if (> (rand) 0.5) 1 2)]
                (mono-mooglad (merge (akwf (weighted
                                            {:dbass 2
                                             :birds 0}))
                                     {:amp 1
                                      :freq (* 160 (mseq i [1 [1 1 17/7] 1 [1 3/2]]))
                                      :offset (+ 0.1)
                                      :range 0.3
                                      :a 0.0
                                      :s 0.01
                                      :moog-freq (* (weighted {800 7}))
                                      :moog-res 0.3
                                      :s-amp 1
                                      :d-level (at-i [0.2 1])
                                      :r 0.3
                                      :out (mseq i (++ 2 [0 2 4]))}))
                (when (xo "xoxoo" i)
                  (mono-mooglad (merge (akwf (weighted
                                              {:dbass 2
                                               :birds 0}))
                                       {:amp 2
                                        :freq (* 80 (mseq i [1 [1 1 17/7]  [1 3/2]]))
                                        :offset (+ 0.1)
                                        :range 0.3
                                        :a 0.0
                                        :s 0.01
                                        :moog-freq (* (weighted {500 7
                                                                 800 3}))
                                        :moog-res 0.3
                                        :s-amp 1
                                        :d-level (at-i [0.2 1])
                                        :r (mseq i (conj (repeat 5 0.3) {(rrange 0.5 1) 1}))
                                        :out (mseq i [0 2 4])})))
                (when (xo "xooooooxooooooooo" i)
                  (mono-mooglad
                   (merge (akwf (weighted
                                 {:dbass 1
                                  :birds 3}))
                          {:amp 2
                           :freq (* 160 3/2 (mseq i (**
                                                     (lin 1 1 2 (* 4/3 17/7))
                                                     (lin 1 [1 1 17/7]  [1 3/2] 2))))
                           :offset (+ 0.1)
                           :range 0.3
                           :a 0.1
                           :s (mseq i (lin 2 2 2 2 2 2 5))
                           :moog-freq (* (weighted {800 7}))
                           :moog-res 0.3
                           :s-amp 1
                           :d-level (at-i [0.2 1])
                           :r (mseq i (conj (repeat 5 0.3) {(rrange 0.5 1) 1}))
                           :out (mseq i (++ 1 [0 2 4]))})))
                (when (xo "xx" i)
                  (let [f (fn [{:keys [period amp]}]
                            (mono-mooglad
                             (merge (akwf (weighted
                                           {:distorted 5
                                            :birds 0}))
                                    {:amp (* amp 2)
                                     :freq (* 160 3 period (mseq i (**
                                                                    (lin 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1
                                                                         4/3
                                                                         4/3
                                                                         4/3
                                                                         4/3
                                                                         4/3
                                                                         4/3)
                                                                    (lin 1 1 1 3 3 3 2 2 1 1 2 2 3 3 3)
                                                                    (lin 17/7 2 (* 17/7 2) 9/4))))
                                     :offset (+ 0.1)
                                     :range 0.3
                                     :a 0.01
                                     :s 0.5
                                     :moog-freq (* (weighted {800 7
                                                              1000 6}))
                                     :moog-res 0.9
                                     :s-amp 1
                                     :d-level (at-i [0.2 1])
                                     :r (mseq i (conj (repeat 5 0.1) {(rrange 0.5 1) 1}))
                                     :out (mseq i (++ 3 [0 2 4 7 8 9 10 (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)
                                                         (graph main-graph)]))})))]
                    (subrain
                     {:ref ::test
                      :ratio 1/5
                      :durs (repeat 8 (mseq i [1]))
                      :on-event (fn [data] (f
                                            {:period (wrap-at i (range 10))
                                             :amp (/ (wrap-at i
                                                              (reverse (range 10)))
                                                     20)}))}))))))

  (rain.v2/stop))
