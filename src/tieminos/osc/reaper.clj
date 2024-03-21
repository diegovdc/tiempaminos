(ns tieminos.osc.reaper
  (:refer-clojure :exclude [time])
  (:require [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn init []
  (when-not @osc-client
    (reset! osc-client (osc/osc-client "0.0.0.0" 1618))
    @osc-client))

(defn time "Set start time at `seconds`"
  [seconds]
  (osc/osc-send @osc-client "/time" (float seconds)))
(defn play [] (osc/osc-send @osc-client "/play"))
(defn stop [] (osc/osc-send @osc-client "/stop"))
(defn rec [] (osc/osc-send @osc-client "/record"))

(defn set-vol [track vol]
  (osc/osc-send @osc-client (format "/track/%s/volume" track) (float vol) ))

(defn basic-insert-marker
  "This is a very simple way to insert markers. It may produce duplicate markers"
  [marker-name]
  (osc/osc-send @osc-client "insert-new-marker") ;; this osc command needs to be asociated to th action "Markers: nsert marker at current position"
  (osc/osc-send @osc-client
                "s/lastmarker/name"
                marker-name))

(comment
  (osc/osc-send @osc-client "/time" (float 121))
  (osc/osc-debug true)
  (osc/osc-debug false)
  (init)
  (play)
  (stop)
  (rec))
