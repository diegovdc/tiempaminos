(ns erv-fib-synth.afable-diablo.experiments.doriqueando
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain stop]]))

(comment
  (do
    (let [scale (->> dorico-1.1 (mapcat (comp polydori-by-sets :set)))]
      (ref-rain :id :test
                :durs [3 2 2 3 2
                       3 2 2 3 2
                       3 2 2 3 2
                       3 2 2 3 3
                       ;; 2 1 2 2 2 1 2
                       ]
                :ratio 1/9
                :on-event
                (on-event
                 (let [deg (weighted {0 10
                                      1 4
                                      2 5
                                      3 3
                                      4 7
                                      5 8
                                      6 4})
                       base-freq (* 440 (weighted {1 5
                                                   2 3
                                                   1/2 4
                                                   1/4 2}))
                       dur (weighted {0.5 1 1 5 1.5 8 2 8 3 2 5 1})]
                   #_(sini :freq (scale/deg->freq scale base-freq deg)
                           :r dur))))

      (ref-rain :id :test2
                :durs [3 2 2 3 2
                       3 2 2 3 2
                       2 1 2 1 1 2 1 2
                       2 1 2 2 2 1 2
                       ]
                :ratio 1
                :on-event
                (on-event
                 (let [deg (weighted {0 10
                                      1 4
                                      2 5
                                      3 3
                                      4 7
                                      5 8
                                      6 4})
                       base-freq (* 440 (weighted {1/8 2}))
                       dur (weighted {0.5 1 1 5 1.5 8 2 8 3 2 5 1})]
                   (sini :freq (scale/deg->freq scale base-freq deg)
                         :r dur))))))

  (stop)
  (o/stop))


(comment
  (o/recording-start "/home/diego/Desktop/doriquiando.wav")
  (o/recording-stop))
