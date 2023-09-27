(ns tieminos.impros.sept232023
  (:require
   [overtone.midi :as midi]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.piraran.scale :refer [polydori-set->deg polydori-v2]]
   [tieminos.utils :refer [map-subscale-degs rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]))

(def sink (midi/midi-out "VirMIDI"))



(comment
  (-> polydori-v2 :subcps
      keys
      sort)
  (def dek-3_1-7-9-15-19
    (-> polydori-v2 :subcps
        (get "3)5 of 4)7 3-1.7.9.15.19")
        :scale
        (->> (map :set)
             (map polydori-set->deg))
        ))

  (-> polydori-v2 :subcps
        (get "3)5 of 4)7 3-1.7.9.15.19")
        :scale
        (->> (map (comp sort :set))
             (map-indexed vector)
             )
        )
  (gp/stop)
  (map-subscale-degs
    29
    dek-3_1-7-9-15-19
    0)
  :rcf)

(defn polydory-degs [subscale degrees]
  (map #(map-subscale-degs 29 subscale %)
       degrees)
)



(comment


  (algo-note   {:sink sink
                :dur  20
                :note (polydory-degs dek-3_1-7-9-15-19 [0 11 6])
                :vel 20})


  (ref-rain
    :id :perc
    :durs [3 5 5]
    :tempo 120
    :ratio 1
    :on-event (on-event
                (algo-note {:chan 1
                            :sink sink
                            :dur 0.5
                            :note (polydory-degs dek-3_1-7-9-15-19
                                                 [(at-index [(at-index [0])])])
                            :vel (at-index [80 60])})))

  (ref-rain
    :id :perc2
    :durs [3 2 3]
    :tempo 120
    :ref :perc
    :ratio 3/2
    :on-event (on-event
                (algo-note {:chan 1
                            :sink sink
                            :dur  (rand-nth [ 5 8 16])
                            :note (polydory-degs dek-3_1-7-9-15-19
                                                 (remove nil?
                                                         [(+ (rrange 18 20) (rand-nth [6 7 8 10 17]))
                                                          #_(+ 25 (at-index [11 10 11 6]))
                                                          #_(when (> 0.5 (rand) (+ 27 (at-index [11 10 11 6]))))]))
                            :vel (rand-nth [100 0 10 80 ])})))
  (ref-rain
    :id :perc3
    :durs [3 2 4]
    :tempo 120
    :ref :perc
    :ratio 1/6
    :on-event (on-event
                #_(algo-note {:chan 1
                              :sink sink
                              :dur  0.3
                              :note (polydory-degs dek-3_1-7-9-15-19
                                                   (remove nil?
                                                           [(+ (at-index [0 0 2 25]) (at-index [11 10 11 6]))
                                                            (when (> 0.5 (rand)) (+ 20 (at-index [1 5 8])))]))
                              :vel (at-index [75 50 10])})))
  (ref-rain
    :id :melody
    :durs [6 5 4 7]
    :on-event (on-event
                #_(algo-note
                    {:sink sink
                     :dur dur-s
                     :note (polydory-degs dek-3_1-7-9-15-19
                                          [(+ (at-index [28 33 36 34 31 32])
                                              (at-index [0 0 -3 0 2 0 -2])
                                              2)]
                                          #_[(+ (at-index [28 27 25 24 31 32])
                                                (at-index [0 0 0 2 0 -2]))])
                     :vel 100
                     :chan 0})))
  (ref-rain
    :id :melody-2
    :ref :melody
    :ratio 20/16
    :durs (concat (repeat 4 4)
                  #_(repeat 60 1/3))
    :on-event (on-event
                #_(algo-note
                    {:sink sink
                     :dur 5
                     :note (polydory-degs dek-3_1-7-9-15-19
                                          (remove nil?
                                                  [#_(+ -22
                                                        (at-index [28 28 26 43])
                                                        (at-index [0 0 0 2 0 -2]))
                                                   (+ -14
                                                      (at-index [28 24 31 43])
                                                      #_(at-index [10 11 12 13 14 15]))
                                                   (+ -9
                                                      (at-index [28 31 33 43])
                                                      #_(at-index [10 11 12 13 14 15]))
                                                   (+ -9
                                                      (at-index [28 31 33 43])
                                                      (at-index [10 11 12 13 14 15]))
                                                   #_(+ -5
                                                        (at-index [28 31 33 43])
                                                        #_(at-index [10 11 12 13 14 15]))]))
                     :vel (at-index [0 20 25 30] )
                     :chan 2})))

  (ref-rain
    :id :melody-3
    :ref :melody
    :ratio 40/16
    :durs (concat (repeat 3 4)
                  (repeat 1 3)
                  #_(repeat 60 1/3))
    :on-event (on-event
                #_(algo-note
                    {:sink sink
                     :dur 20
                     :note (polydory-degs dek-3_1-7-9-15-19
                                          [(+ -9
                                              (at-index [28 25])
                                              (at-index [0 0 3 -2 0]))
                                           (+ -3
                                              (at-index [28 25])
                                              (at-index [0 0 3 -2 0]))
                                           (+
                                             (at-index [28 25])
                                             (at-index [0 0 3 -2 0]))])
                     :vel (at-index [40 60 30] )
                     :chan 2}))))
