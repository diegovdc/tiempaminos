(ns tieminos.habitat.scratch.sample-rec2
  (:require
   [erv.cps.core :as cps]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.recording :as rec :refer [norm-amp rec-input]]
   [tieminos.habitat.routing :refer [inputs mixed-main-out]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]))

(defn bus [k] (-> @inputs k :bus))

(comment
  (init!)
  (timbre/set-level! :info)
  (let [buf (-> @rec/bufs vals rand-nth)]
    (o/demo
     (* (norm-amp buf)
        (o/play-buf 1 buf))))

  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "guitar"
              :input-bus (bus :guitar)
              :dur-s (rrange 7 15)})
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "mic-1"
              :input-bus (bus :mic-1)
              :dur-s (rrange 7 15)})

  (do
    (oe/defsynth s1
      [buf 0
       amp 1
       a 0.1
       s 2
       r 0.5
       start-pos 0
       room 2
       rate 1
       out 0]
      (o/out out (-> (o/play-buf 1 buf :rate rate :start-pos start-pos)
                     (#(o/pan-az 4 % (lfo 0.2 -1 1)))
                     (o/free-verb 0.8 room)
                     (* amp (o/env-gen (o/envelope [0 1 1 0] [a s r])
                                       :action o/FREE)))))
    (s1 {:buf (-> @rec/bufs vals rand-nth)
         :start-pos (-> @rec/bufs vals rand-nth :n-samples rand-int)}))

  (-> @rec/bufs vals rand-nth :n-samples rand-int)
  (def ratios (->> (cps/make 2 [9 13 15 21]) :scale (map :bounded-ratio)))
  (gp/stop)
  (ref-rain
   :id ::sample-rec
   :durs [1]
   :on-event (on-event
              (s1 {:buf (-> @rec/bufs vals rand-nth)
                   :start-pos (-> @rec/bufs vals rand-nth :n-samples rand-int)
                   :s (rrange 5 10)
                   :r (rrange 1 3)
                   :amp (rrange 0.8 1.2)
                   :room (rrange 0.5 2)
                   :rate (* (rand-nth (conj ratios 1))
                            (rand-nth [1 1/2 1/4 2 4]))
                   :out mixed-main-out}))))
