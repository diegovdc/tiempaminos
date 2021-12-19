(ns tieminos.afable-diablo.experiments.doriqueando
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [tieminos.afable-diablo.dorian-scales :as ds]
   [tieminos.afable-diablo.scale :refer [polydori-by-sets polydori]]
   [tieminos.afable-diablo.synths :refer [sini]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain stop]]
   [clojure.string :as str]))
(def anti-dorico-1v1
  (let [dorico (set (map :set ds/dorico-1v1))]
    (->> polydori :scale (remove #(dorico (:set %))))))

(-> anti-dorico-1v1)
(->> polydori :subcps (filter #(str/includes? % "3)5" )))
(comment
  (do
    (let [scale (->> ds/dorico-1v1 (mapcat (comp polydori-by-sets :set)))
          scale anti-dorico-1v1]
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
                 (let [deg (- (rand-int 20)
                              (weighted {0 10
                                         1 4
                                         2 5
                                         3 3
                                         4 7
                                         5 8
                                         6 4
                                         ;; 16 9
                                         ;; 20 5
                                         ;; 22 6
                                         }))
                       base-freq (* 880 (weighted {1 5
                                                   2 3
                                                   1/2 4
                                                   1/4 2
                                                   ;; 1/8 2
                                                   }))
                       dur (* 5 (weighted {0.5 0 1 5 1.5 8 2 8 3 2 5 1}))]
                   (sini :freq (scale/deg->freq scale base-freq deg)
                         ;; :a dur
                         :r dur :amp 0.1))))

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
                       dur (* 3 (weighted {0.5 1 1 5 1.5 8 2 8 3 2 5 1}))]
                   (sini :freq (scale/deg->freq scale base-freq deg)
                         :r dur))))))

  (stop)
  (o/stop))


(comment
  (o/recording-start "/home/diego/Desktop/doriquiando.wav")
  (o/recording-stop))
