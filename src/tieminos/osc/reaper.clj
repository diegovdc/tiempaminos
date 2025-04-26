(ns tieminos.osc.reaper
  (:refer-clojure :exclude [time])
  (:require [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn init []
  (when-not @osc-client
    (reset! osc-client (osc/osc-client "0.0.0.0" 65432))
    @osc-client))

(defn time "Set start time at `seconds`"
  [seconds]
  (osc/osc-send @osc-client "/time" (float seconds)))
(defn play [] (osc/osc-send @osc-client "/play"))
(defn stop [] (osc/osc-send @osc-client "/stop"))
(defn rec [] (osc/osc-send @osc-client "/record"))

(defn set-vol
  "0.7158 is approximately 0db"
  [track vol]
  (osc/osc-send @osc-client (format "/track/%s/volume" track) (float vol)))

(def reaper-db {:-inf 0
                -6 0.59
                -3 0.649
                0 0.7158
                3 0.79
                6 0.86})

(defn from-db [db]
  (if-let [vol (reaper-db db)]
    vol
    (throw (ex-info "Unknown db value" {:db db :available-values (keys reaper-db)}))))

(defn set-track-rec [track arm?]
  (osc/osc-send @osc-client (format "/track/%s/recarm" track) (int (if arm? 1 0))))

(defn set-autotrim
  "Set automation mode to trim"
  [track]
  (osc/osc-send @osc-client (format "/track/%s/autotrim" track)))

(defn set-autowrite
  "Set automation mode to write"
  [track]
  (osc/osc-send @osc-client (format "/track/%s/autowrite" track)))

(comment
  (set-vol 1 0.59)
  (set-track-rec 22 false))

(defn basic-insert-marker
  "This is a very simple way to insert markers. It may produce duplicate markers"
  [marker-name]
  (osc/osc-send @osc-client "insert-new-marker") ;; this osc command needs to be asociated to th action "Markers: insert marker at current position"
  (osc/osc-send @osc-client
                "s/lastmarker/name"
                marker-name))

(comment
  (reset! osc-client nil)
  (osc/osc-send @osc-client "/time" (float 121))
  (osc/osc-debug true)
  (osc/osc-debug false)
  (init)
  (play)
  (stop)
  (rec)
  (basic-insert-marker "Tieminos Marker"))

;; Helper functions
(def ^:private reaeq-freq->lin-map
  {0     0
   300   0.29
   600   0.395
   1000  0.476
   2000  0.589
   3000  0.655
   5000  0.74
   10000 0.855
   15000 0.922
   24000 1})

(comment
  (require '[tieminos.habitat.osc :as habitat-osc])
  ;; to find a new frequency: on a new reaper proyect insert an eq on track 1
  (osc/osc-send
   @habitat-osc/reaper-client
   "/track/1/fxeq/loshelf/freq"
    ;; use a value between 0 - 1
   (float 1)))

(defn reaeq-freq->lin
  "Convert reaeq freqs to their linear approximate representation"
  ;; NOTE only certain frequencies are represented
  [freq]
  (if-let [lin-val (reaeq-freq->lin-map freq)]
    lin-val
    (throw (ex-info "Unknown frequency"
                    {:freq freq
                     :freq-map reaeq-freq->lin-map}))))
