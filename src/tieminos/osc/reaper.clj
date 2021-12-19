(ns tieminos.osc.reaper
  (:require [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn init [] (reset! osc-client (osc/osc-client "192.168.10.105" 8002)))

(defn play [] (osc/osc-send @osc-client "/play"))
(defn stop [] (osc/osc-send @osc-client "/stop"))
(defn rec [] (osc/osc-send @osc-client "/record"))

(comment
  (osc/osc-debug true)
  (osc/osc-debug false)
  (init)
  (play)
  (stop)
  (rec))
