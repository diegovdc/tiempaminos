(ns tieminos.afable-diablo.experiments.doriqueando
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [tieminos.afable-diablo.base
    :refer [act-level elapsed-time get-out make-activity-map]]
   [tieminos.afable-diablo.dorian-scales :as ds]
   [tieminos.afable-diablo.harmonic-clock :as hc]
   [tieminos.afable-diablo.scale :refer [polydori root]]
   [tieminos.afable-diablo.synths :refer [sini]]
   [tieminos.synths :refer [low sharp-plate]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain stop]]))

(def anti-dorico-1v1
  (let [dorico (set (map :set ds/dorico-1v1))]
    (->> polydori :scale (remove #(dorico (:set %))))))

(-> anti-dorico-1v1)
(->> polydori :subcps (filter #(str/includes? % "3)5")))

(comment
  (do
    (let [scale (:scale @hc/momento)
          act-map (make-activity-map (:act-seq @hc/momento))
          lvl (act-level act-map @elapsed-time)]
      (ref-rain :id :test3
                :durs [3 2 2 3 2
                       3 2 2 3 2
                       3 2 2 3 2
                       3 2 2 3 3
                       2 1 2 2 2 1 2]
                :ratio 1/9
                :on-event
                (on-event
                 (let [deg (- (weighted {0 10
                                         1 4
                                         2 5
                                         3 3
                                         4 7
                                         5 8
                                         6 4
                                         8 4
                                         9 4
                                         16 9
                                         ;; 20 5
                                         ;; 22 6
                                         })
                              (weighted (assoc {0 15}
                                               (* -1 (inc (rand-int 20))) 0)))
                       base-freq (* root 4 (weighted {1 5
                                                      2 5
                                                      1/2 4
                                                      ;; 1/4 2
                                                      ;; 1/8 2
                                                      }))
                       dur (* 3 (weighted {0.5 0
                                           1 5
                                           1.5 8
                                           2 8
                                           3 2
                                           5 1}))
                       freq (scale/deg->freq scale base-freq deg)]
                   (when (> 0.1 (rand))
                     ;; TODO move this to another refrain
                     (low  :freq (/ freq 4)
                           :mod-freq (rand-nth [2000 500 9000])
                           :amp 0.3
                           :dcy 4
                           :out (get-out :dq-highs)))
                   (when (> lvl (rand))
                     ;; TODO better that sini? ... maybe both growing in probability
                     (sharp-plate
                      :freq (* (weighted {1/8 10
                                          1/16 20
                                          1/32 30
                                          ;; 1 1
                                          })freq)
                      :mod-freq (rand-nth [2000 5000 9000
                                           ;; after a while
                                           200
                                           300])
                      :amp (rrange 0.01 0.3)
                      :atk (weighted (assoc {1 8
                                             0.1 25 #_150
                                             ;; 0.05 25 #_150
                                             }
                                            dur 5))
                      :dcy (* 0.5 dur)
                      :out (get-out :dq-highs)))
                   (when (> (*  lvl) (rand))
                     (sini :freq freq
                           :pan-speed (* 20 (rand))
                           :a (weighted (assoc {;; 1 8
                                                ;; 0.1 10
                                                ;; 0.05 25 #_150
                                                }
                                               dur 10))
                           :r dur
                           :amp (rrange 0.01 0.5)
                           :out (get-out :dq-highs))))))

      (ref-rain :id :test2
                :durs [3 2 2 3 2
                       3 2 2 3 2
                       2 1 2 1 1 2 1 2
                       2 1 2 2 2 1 2]
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
                       base-freq (* root  (weighted {1/8 2 1/4 2}))
                       dur (* 4 (weighted {0.5 1 1 5 1.5 8 2 8 3 2 5 1}))]
                   (sini :freq (scale/deg->freq scale base-freq deg)
                         :amp 0.7
                         :a 3
                         :r dur
                         :out (get-out :dq-bass)))))))

  (stop :test2)
  (o/stop))

(comment
  (o/recording-start "/home/diego/Desktop/piraran-ensayo2.wav")
  (o/recording-start "/home/diego/Desktop/doriquiando.wav")
  (o/recording-stop))
