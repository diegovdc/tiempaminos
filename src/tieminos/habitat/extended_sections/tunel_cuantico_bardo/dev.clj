(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.dev
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms :refer [dispatch]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.habitat.recording :refer [bufs]]
   [time-time.dynacan.players.gen-poly :as gp]))
;;;;;;;;;;;;;;;;;;
;; Controls
;;;;;;;;;;;;;;;;;;
(comment
  ;; osc
  (osc/osc-debug false)
  ;; rec
  (dispatch {:type :start-recording :data {:input-k :mic-1}})
  (dispatch {:type :stop-recording :data {:input-k :mic-1}})

  ;; play
  (dispatch {:type :dev/trigger-clouds-event :data {:bank 0}})

  (-> @gp/refrains keys)
  (-> @gp/refrains :bardo.clouds/diego1)
  (gp/stop))

;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;

(comment
  (bardo.live-state/get-player-data :milo 2)
  (-> (bardo.live-state/get-player-data :diego)
      :refrains)
  (apply dissoc (bardo.live-state/get-player-data :milo) (range 8))

  (bardo.live-state/get-selected-synth-data :diego)
  (bardo.live-state/get-player-data :diego :refrains))

(comment
  (-> @bufs first second keys)
  (-> @bufs first second :rec/meta)
  (do
    (def bufs*
      (->> @bufs
           vals
           #_(map #(-> % :rec/meta :input-name))
           (filter #(-> % :rec/meta :input-name #{"mic-2-bus"}))
           #_(map :rec/meta)))
    (->> bufs*
         (map :rec/meta)))

  (o/defsynth bufy
    [buf 0
     amp 1]
    (o/out 0 (* amp (o/play-buf 1 buf :action o/FREE))))

  (o/defsynth inputy
    [in 0
     amp 1
     out 0]
    (o/out out (* amp (o/sound-in in))))
  (def inp (inputy :in 22
                   ;; :amp (o/db->amp 6)
                   ))
  (o/kill inp)
  (o/stop)

  (bufy (first bufs*)
        (* 8 64))
  (->> bufs*
       first
       (into {})))
