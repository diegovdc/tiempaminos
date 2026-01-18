(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.dev
  (:require
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms :refer [dispatch]]))

(comment
  ;; rec
  (dispatch {:type :start-recording :data {:input-k :mic-1}})
  (dispatch {:type :stop-recording :data {:input-k :mic-1}})

  ;; play
  (dispatch {:type :dev/trigger-clouds-event :data {:bank 0}}))
