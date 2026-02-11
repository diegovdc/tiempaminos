(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.exp-1
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.utils.core :refer [period-reduce]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-space
    :refer [main-graph]]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.synths
    :refer [low]]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.utils
    :refer [subrain]]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.wavetable
    :refer [akwf-samples available-dirs]]
   [tieminos.seq-utils.core :refer [** choose graph lin mseq xo]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.refrain.v2
    :as
    rain.v2
    :refer
    [on-event ref-rain]]))

(comment
  (def akwf (apply akwf-samples (available-dirs)))
  (ref-rain
   :id :test
   :durs [1/8]
   :on-event (on-event
              (let [out1  (mseq i [16 10 0])
                    out2  (mseq i [13 1 18]) #_(mseq i (graph tri6-1))
                    mult1 (if (even? i) 1 1/2)
                    mult2 (if (> (rand) 0.5) 1 2)]
                (if (xo "xxooxoooxo" i)
                  (low :freq (* mult1 (at-i [200 300 200 150]))
                       :dcy (mseq i {0.5 7
                                     1   2
                                     2   1
                                     5 1})
                       :amp (* (rand-nth [1 2 3]) 0.3)
                       :out out1)
                  (subrain
                   (let [fs (reverse (concat [200 300 200 150 700]
                                             (rand-nth [[900 1100]
                                                        []
                                                        [1300]
                                                        [50 75]
                                                        [1300 1500 1400 1400 1200 600 1500 1600 1700 1800]])))
                         out (+ (mseq i [1 3 5 7])
                                (mseq i [1 0])

                                out1)
                         mult3    (mseq i (lin 1 2 3 4))
                         bass-freq #(mseq % (** (lin 1 1 1 2 1 1 1 2 1 1 3/2 7/4)
                                                [20 60 40 80 60]))
                         xo-offset (mseq i [0 1 2 3 4 5])]
                     {:durs     (repeat (count fs) (mseq i (** (choose 2 3 5 7) (choose 1/4 1/8 1/2 1/16 1 2/13 1/3))))
                      :on-event (on-event
                                 (low :freq (* 2 mult3 (at-i fs))
                                      :atk (mseq i {(rrange 0.01 0.05) 5
                                                    (rrange 0.05 0.5) 1
                                                    (rrange 0.05 1) 1/2})
                                      :dcy (mseq i {0.2 1
                                                    0.5 3
                                                    1   2
                                                    2   1
                                                    5 1/2})
                                      :mod-freq (rationalize (rrange 1 200))
                                      :amp (* (rand-nth [1]) 0.1)
                                      :out (mseq i (graph main-graph)))
                                 (when (xo "xoxoo" xo-offset)
                                   (low :freq (* 2 (->> (/ (at-i fs)
                                                           200)
                                                        (period-reduce)
                                                        (* (bass-freq i))))
                                        :atk (mseq i {0.01 10 5 1/4})
                                        :dcy (* (/ dur 2) (mseq i {0.2 5
                                                                   1   5
                                                                   4   2
                                                                   6   1}))
                                        :mod-freq (rrange 500 2000)
                                        :amp (* (weighted {0.8 10
                                                           0.6 7})
                                                (rand-nth [1 2])
                                                1)
                                        :out (mod (+ out) 24))))})))
                #_(when (xo "xoooooxoxo" i)
                    (low :freq (* mult2 (at-i [300 200 1100 150]))
                         :dcy 0.5
                         :amp 0.3
                         :out out2))
                #_(when (xo "xooooooo" i)
                    (ref-rain
                     :id (random-uuid)
                     :ratio (at-i [1/2 3 1])
                     :durs (at-i  [[1 2 3]])
                     :on-event (fn [_]
                                 (low :freq (* 1/4 (weighted {1 10
                                                              2 1
                                                              4 1})
                                               (* 3 (at-i [300 200 150 400 700])))
                                      :dcy 0.2
                                      :amp 0.05
                                      :out (mseq i (map dec [7 8 6]))))))
                #_(when (xo "xoooooooooxooxo" i)
                    (ref-rain
                     :id (random-uuid)
                     :ratio (at-i [1/2 3 1])
                     :durs (at-i  [[1 1]
                                   [2 2 2]
                                   [3 2 2]])
                     :on-event (fn [_]
                                 (low :freq (* 1/4 (weighted {1 10
                                                              2 1
                                                              4 1})
                                               (* 7/4 (mseq i [100 200 150 {1100 3 700 5}])))
                                      :mod-freq (rrange 90 190)
                                      :atk (mseq i {0.01 3 1 1})
                                      :dcy (mseq i {0.2 5
                                                    1   1
                                                    2   1})
                                      :amp (mseq i [0.2 0.3 0.2 0.1])
                                      :out (+ (mseq i [3 3 3 3  4 4 4 4 4])
                                              (mseq i [7 8 6])))))))))

  (rain.v2/stop))
