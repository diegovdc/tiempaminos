(ns tieminos.osc.reaper
  (:refer-clojure :exclude [time])
  (:require
   [clojure.core.async :as async]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.network-utils :refer [get-local-host]]
   [tieminos.utils :refer [sequence-calls2]]))

(defonce osc-client (atom nil))

(defn init []
  (when-not @osc-client
    (reset! osc-client (osc/osc-client
                        #_(get-local-host)
                        "127.0.0.1" ;; this one seems to work for now
                        #_"0.0.0.0"
                        65432))
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

(def zero-db 0.7158)

(def reaper-db {:-inf 0
                -6 0.59
                -3 0.649
                0 zero-db
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

(defn set-autoread
  "Set automation mode to read"
  [track]
  (osc/osc-send @osc-client (format "/track/%s/autoread" track)))

(defn set-autowrite
  "Set automation mode to write"
  [track]
  (osc/osc-send @osc-client (format "/track/%s/autowrite" track)))

(comment
  (set-vol 1 0.59)
  (set-track-rec 22 false))

(defn set-fx
  "Set fx param"
  [track fx param val]
  ;; n/track/@/fx/@/fxparam/@/value
  (osc/osc-send @osc-client (format "/track/%s/fx/%s/fxparam/%s/value" track fx param) (float val)))

(comment
  (set-fx 2 2 1 0.5)
  (set-fx 2 2 1 0.52)
  (set-fx 2 2 1 0.54)
  (set-fx 2 2 1 0.56)
  (set-fx 2 2 1 0.58)
  (set-fx 2 2 1 0.60))

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
  ;; NOTE: only certain frequencies are represented
  [freq]
  (if-let [lin-val (reaeq-freq->lin-map freq)]
    lin-val
    (throw (ex-info "Unknown frequency"
                    {:freq freq
                     :freq-map reaeq-freq->lin-map}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FX toggling and track selection
;; NOTE it is advisable that for all
;; these operations, that go blocks to
;; control timing is several OSC
;; operations are performed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn select-track
  [track select?]
  (osc/osc-send @osc-client (format "/track/%s/select" track) (float (if select? 1 0))))

(comment (select-track 1 true)
         (select-track 2 true))

(defn unselect-all-tracks
  "Calls `SWS: Unselect all items/tracks/env points`.
  The endpoint is a custom path added to the action."
  []
  (osc/osc-send @osc-client "/sws-unselect-all-tracks"))

(defn just-select-track
  "Only selects the given track, and unselects the rest."
  [track]
  (unselect-all-tracks)
  (select-track track true))

(comment (unselect-all-tracks))

(defn toggle-selected-tracks-fx
  "NOTE: it is a good idea to `unselect-all-tracks` before calling this."
  [on?]
  (if on?
    (osc/osc-send @osc-client "/action"  "_SWS_UNBYPASSFX")
    (osc/osc-send @osc-client "/action"  "_SWS_BYPASSFX")))

(comment (toggle-selected-tracks-fx false)
         (toggle-selected-tracks-fx true))

(defn toogle-tracks-fx*
  "Will first unselect all tracks, then select the given tracks then turn them on or off, then unselect them.
  NOTE if this is called several times in a very short span of time, the behavior may be unpredictable as the calls may interfere with each other.

  See `make-toogle-tracks-fx`."
  [on? tracks]
  (async/go
    (try
      (unselect-all-tracks)
      (doseq [track tracks]
        (select-track track true))

      ;; Account for the time it takes REAPER to process the above.
      (async/<! (async/timeout 100))
      (toggle-selected-tracks-fx on?)
      (unselect-all-tracks)
      (catch Exception e (timbre/error "Error in toggle-tracks-fx*" e)))))

(defn make-toogle-tracks-fx
  "Make a toggler that can safely sequence multiple calls to `toggle-tracks-fn*`. It's kept in a function to aviod unnecessarily creating async chan and go-loops."
  []
  (sequence-calls2 toogle-tracks-fx* 200))

(comment
  (def toggle-fx (make-toogle-tracks-fx))
  (do (toggle-fx true (range 1 5))
      (toggle-fx false (range 5 12)))
  (do (toggle-fx false (range 1 5))
      (toggle-fx true (range 5 12))))

(defn remove-all-envelopes
  "Calls `SWS/S&M: Remove all envelopes for all tracks`
  The endpoint is a custom path added to the action."
  []
  (osc/osc-send @osc-client "/sws-remove-all-envelopes"))

(comment (remove-all-envelopes))

;;;;;;;;;;;;;;;
;; FX windows
;;;;;;;;;;;;;;

(defn close-all-fx-chain-windows
  "Calls `SWS/S&M: Close all fx chain windows`
  The endpoint is a custom path added to the action."
  []
  (osc/osc-send @osc-client "/sws-close-all-fx-chain-windows"))

(defn close-all-fx-windows
  "Calls `SWS/S&M: Close all fx windows`
  The endpoint is a custom path added to the action."
  []
  (osc/osc-send @osc-client "/sws-close-all-fx-windows"))

(defn close-all-fx-windows-except-focused
  "Calls `SWS/S&M: Close all fx windows, except focused-one`
  The endpoint is a custom path added to the action."
  []
  (osc/osc-send @osc-client "/sws-close-all-fx-chain-windows-except-focused"))

(comment
  (close-all-fx-chain-windows)
  (close-all-fx-windows)
  (close-all-fx-windows-except-focused))
