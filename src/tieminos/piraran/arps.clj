(ns tieminos.piraran.arps
  (:require
   [erv.scale.core :refer [deg->freq]]
   [erv.utils.core :refer [wrap-at]]
   [overtone.core :as o]
   [tieminos.math.bezier :as bz]
   [tieminos.math.utils :refer [linexp]]
   [tieminos.piraran.base :refer [get-out]]
   [tieminos.piraran.harmonic-clock :refer [dek-seq momento]]
   [tieminos.piraran.scale :refer [root]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(defn scale-durs [total-dur durs]
  (let [max-dur (apply + durs)]
    (map #(* total-dur (/ % max-dur)) durs)))

(comment
  (reset! momento (nth dek-seq 3))
  (do
    (o/defsynth sharp-plate
      [freq 350
       amp 1
       mod-freq 300
       pan 0
       atk 0.01
       dcy 0.5
       out 0]
      (o/out out (-> (o/range-lin
                      (* (o/saw mod-freq) (o/env-gen (o/env-perc atk dcy)))
                      (- freq 450)
                      (+ freq 450))
                     o/sin-osc
                     (o/rlpf 3000)
                     (o/pan2 pan)
                     (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                     (* amp))))


    (let [start (- (rand-int 20) 40)
          degs ((rand-nth [identity reverse shuffle])
                (range start (+ start 40) (inc (rand-int 7))))
          scale (:scale @momento)
          total-dur (or  (+ 1 (rand-int 5)) 0.1)
          durs ((rand-nth [identity reverse]) (scale-durs total-dur (linexp 0.05 0.4 (bz/curve (count degs) [1 3 13 7]))))
          amp 0.8
          amps ((rand-nth [identity reverse]) (linexp 0.05 0.5 (bz/curve (count degs) [1 3 13 7])))
          dcys (linexp 0.05 10 (bz/curve (count degs) [1 3 13 7]))]
      (ref-rain :id (keyword "arp" (str (rand-int 60000)))
                :durs durs
                :loop? false
                :on-event (on-event
                           (let [freq (deg->freq scale root (wrap-at index degs))]
                             (sharp-plate freq :mod-freq (+ 200 (rand-int 400)) :pan (- (rand 2) 1) :amp (* amp (wrap-at index amps)) :dcy (wrap-at index dcys) :out (get-out :arps))))))))
