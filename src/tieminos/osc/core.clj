(ns tieminos.osc.core
  "Communication with SuperCollider for interaction with MIDI sources and sinks"
  (:require [overtone.osc :as osc]
            [overtone.music.time :refer [apply-at]]
            [overtone.sc.node :refer [ctl node? node-live?]]
            [time-time.standard :refer [now]]
            [taoensso.timbre :as timbre]))


;;;;;;;;;;;;;;;;;;;;
;;; Receive MIDI ;;;
;;;;;;;;;;;;;;;;;;;;

(defonce synths (atom {}))

(defn add-synth [ev synth]
  (when (or (node? synth)
            (and (seq synth) (every? node? synth)))
    (swap! synths assoc (:note ev) synth)))


(defn remove-synth [ctl ev]
  (let [synth (@synths (:note ev)) ]
    (cond
      (node? synth) (do (ctl synth :gate 0)
                        (swap! synths dissoc (:note ev)))
      (and (seq synth) (every? node? synth)) (do (doseq [s synth] (ctl s :gate 0))
                                                 (swap! synths dissoc (:note ev)))
      :else nil)))

(defn get-auto-gate-ctl [auto-ctl?]
  (if auto-ctl?
    {:add add-synth :remove (partial remove-synth ctl)}
    {:add (fn [_ _] nil) :remove (fn [_] nil)}))

(def midi-event-config (atom {:note-on (fn [_] nil)
                              :note-off (fn [_] nil)
                              :auto-ctl? true}))

(defn as-midi-msg [args]
  (let [[note velocity chan] args] {:note note :vel velocity :chan chan}) )

(do
  (defn midi-event-listener [server]
    (osc/osc-listen
     server
     (fn [{:keys [path args]}]
       (let [{:keys [note-on note-off auto-ctl?]} @midi-event-config
             msg (as-midi-msg args)
             gate-ctl (get-auto-gate-ctl auto-ctl?)]
         (case path
           "/note-on" ((gate-ctl :add) msg (note-on msg))
           "/note-off" (do (note-off msg)
                           ((gate-ctl :remove) msg))
           (timbre/warn "OSC path not defined: " path ))))
     :midi-event))
  (comment (midi-event-listener (recv :server))))

(defn init [port]
  (let [server (osc/osc-server port)
        listener (midi-event-listener server)]
    (timbre/info (str "OSC receiver initialized on port: " port))
    {:server server :listener listener :port port}))

(defn init-server [port]
  (let [server (osc/osc-server port)]
    (timbre/info (str "OSC receiver initialized on port: " port))
    {:server server :port port}))

(defn midi-event
  [& {:keys [note-on note-off auto-ctl?]
      :or {note-off (fn [_] nil)
           auto-ctl? true}}]
  (reset! midi-event-config
          {:note-on note-on
           :note-off note-off
           :auto-ctl? auto-ctl?} ))

(comment (defonce recv (init 7777)))


;;;;;;;;;;;;;;;;;
;;; Send MIDI ;;;
;;;;;;;;;;;;;;;;;

(defonce midi-send-client (osc/osc-client "localhost" 7778))

(defn send-note-on
  [& {:keys [note vel bend chan]
      :or {chan 0 vel 60}}]
  (osc/osc-send midi-send-client "/send-midi" "note-on"
                (int note)
                (int vel)
                (int chan)
                (when bend (int bend))))

(defn send-note-off
  [& {:keys [note vel chan]
      :or {chan 0 vel 0}}]
  (osc/osc-send midi-send-client "/send-midi" "note-off"
                (int note)
                (int vel)
                (int chan)))

(defn schedule-note-off
  ([after-ms note vel] (schedule-note-off note vel 0))
  ([after-ms note vel chan]
   (apply-at (+ after-ms (now))
             #(send-note-off :note note :vel 0 :chan chan))))

(defn send-note*
  "Will send a `note-on` and a `note-off` `msg` after `dur`"
  ([& {:keys [dur note vel chan bend]
       :or {chan 0 vel 60}}]
   (send-note-on :note note :vel vel :bend bend :chan chan)
   (schedule-note-off dur note vel chan)))

(defn all-notes-off []
  (osc/osc-send midi-send-client "/send-midi" "all-notes-off"))
(defn reset-pitch-bend []
  (osc/osc-send midi-send-client "/send-midi" "reset-pitch-bend"))

(comment
  (osc/osc-debug true)
  (send-note* :dur 1000 :note 63 :vel 60 :bend 0)
  (all-notes-off)
  (reset-pitch-bend))



(comment
  (require '[erv-fib-synth.synths :as synths])
  (midi-event :note-on (fn [msg] (synths/low)))
  (osc/osc-close (recv :server))
  (osc/osc-close)
  (midi-event :note-on println :note-off println ))
(comment (osc/osc-rm-all-listeners server))
