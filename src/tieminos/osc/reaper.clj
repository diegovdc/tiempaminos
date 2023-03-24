(ns tieminos.osc.reaper
  (:require [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn init []
  (when-not @osc-client
    (reset! osc-client (osc/osc-client "0.0.0.0" 1618))
    @osc-client))

(defn time [seconds]
  "Set start time at `seconds`"
  (osc/osc-send @osc-client "/time" (float seconds)))
(defn play [] (osc/osc-send @osc-client "/play"))
(defn stop [] (osc/osc-send @osc-client "/stop"))
(defn rec [] (osc/osc-send @osc-client "/record"))

(comment
  (osc/osc-send @osc-client "/time" (float 121))
  (osc/osc-debug true)
  (osc/osc-debug false)
  (init)
  (play)
  (stop)
  (rec))
