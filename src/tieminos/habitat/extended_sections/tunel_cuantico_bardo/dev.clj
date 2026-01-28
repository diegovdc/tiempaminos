(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.dev
  (:require
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms :refer [dispatch]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [time-time.dynacan.players.gen-poly :as gp]))
;;;;;;;;;;;;;;;;;;
;; Controls
;;;;;;;;;;;;;;;;;;
(comment
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
  (bardo.live-state/get-player-data :milo 0)
  (apply dissoc (bardo.live-state/get-player-data :milo) (range 8))

  (bardo.live-state/get-player-data :diego :refrains))
