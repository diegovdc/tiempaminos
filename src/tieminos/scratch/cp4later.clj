(ns tieminos.scratch.cp4later
  (:require
   [erv.scale.core :refer [deg->freq]]
   [tieminos.seq-utils.core :refer [rainseq]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.synths :as s]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

;; based on
(comment
  (def ps (chord-progression {:scale-size 11
                              :scale-steps [2 1 1 1 2 1 1 1 1]
                              :chords-steps #{[3,2,3,3]
                                              [3,2,4,2]
                                              [4,3,2,2]
                                              [3,4,2,2]}
                              :max-len 3
                              :max-movement 3
                              :initial-chord-degs [0 3 5 7]})))

(comment
  (do
    (def chords (->> '(([0 3 5 7] (-1 2 5 8) (-2 2 4 7))
                       ([0 3 5 7] (-1 2 5 7) (0 3 5 8))
                       ([0 3 5 7] (0 2 4 7) (-2 2 5 7))
                       ([0 3 5 7] (-1 2 5 7) (-2 2 4 7))
                       ([0 3 5 7] (0 2 4 8) (-1 2 5 7))
                       ([0 3 5 7] (-1 3 5 8) (-1 2 4 8))
                       ([0 3 5 7] (0 2 5 8) (-1 2 4 7))
                       ([0 3 5 7] (-2 2 5 7) (0 2 4 7))
                       ([0 3 5 7] (-1 2 4 7) (-1 3 5 7))
                       ([0 3 5 7] (-1 3 5 8) (0 2 5 7))
                       ([0 3 5 7] (0 2 4 7) (0 3 5 8))
                       ([0 3 5 7] (-1 2 5 8) (0 2 4 8)))
                     #_(wrap-at 18)
                     #_rand-nth
                     flatten))
    (println chords)
    #_(println (->> chords (map (fn [chord] (mapv #(deg->freq scale 1 %) chord)))
                    (map intervals)))
    (rain.v2/ref-rain
     :id :test
     :durs [0.5
            0.5
            0.5 0.5]
     :ratio 1/4
      ;; :loop? false
     :on-event (rain.v2/on-event
                (let [chord [(->> (rainseq chords))]

                      t (rainseq (repcat [7 4]
                                         [7 7]
                                         [7 [-7 3]]
                                         [7 [22 25]]))]

                  (when (#{1} (mod i 6))
                    (doseq [% chord]
                      (s/low (deg->freq scale 220 (+ t %))
                             #_#_:mod-freq (* freq 7)
                             :atk 3 :dcy 4))))))
    (rain.v2/ref-rain
     :id :test2
     :ref :test
     :durs [0.5
            0.5
            0.5 0.5]
     :ratio 1/4
      ;; :loop? false
     :on-event (rain.v2/on-event
                (let [chord [(->> (rainseq chords))]

                      t (- (rainseq (repcat [7 4]
                                            [7 7]
                                            [7 [-7 3]]
                                            [7 23]))
                           11)]

                  #_(when (or (#{} (mod i 5))
                                ;; true
                              (#{} (mod i 7))
                              (#{4} (mod i 9)))
                      (doseq [% chord]
                        (s/low (deg->freq scale 220 (+ t %))
                               :mod-freq  7777
                                 ;; :atk 3
                               :dcy 0.7))))))))


