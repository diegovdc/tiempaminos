(ns erv-fib-synth.osc-midi-receiver
  (:require [overtone.osc :as osc]
            [overtone.music.time :refer [apply-at]]
            [overtone.sc.node :refer [ctl node?]]
            [time-time.standard :refer [now]]
            [taoensso.timbre :as timbre]))

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
       #_(println path)
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
    (timbre/info "OSC receiver initialized on port: 7777")
    {:server server :listener listener :port port}))

(defn midi-event
  [& {:keys [note-on note-off auto-ctl?]
      :or {note-off (fn [_] nil)
           auto-ctl? true}}]
  (reset! midi-event-config
          {:note-on note-on
           :note-off note-off
           :auto-ctl? auto-ctl?} ))

(defonce recv (init 7777))


(comment)

(defonce midi-send-client (osc/osc-client "localhost" 7778))

(defn send-note-on
  ([note vel] (send-note-on note vel 0))
  ([note vel chan]
   (osc/osc-send midi-send-client "/send-midi" "note-on"
                 (int note)
                 (int vel)
                 (int chan))))

(defn send-note-off
  ([note vel] (send-note-on note vel 0))
  ([note vel chan]
   (osc/osc-send midi-send-client "/send-midi" "note-off"
                 (int note)
                 (int vel)
                 (int chan))))

(defn schedule-note-off
  ([after-ms note vel] (schedule-note-off note vel 0))
  ([after-ms note vel chan]
   (apply-at (+ after-ms (now))
             #(send-note-off note vel chan))))

(defn send-note*
  "Will send a `note-on` and a `note-off` `msg` after `dur`"
  ([dur note vel] (send-note* dur note vel 0))
  ([dur note vel chan]
   (send-note-on note vel chan)
   (schedule-note-off dur note vel chan)))

(defn all-notes-off []
  (osc/osc-send midi-send-client "/send-midi" "all-notes-off"))

(comment
  (osc/osc-debug false)

  (send-note* 5000 63 16)
  (all-notes-off))



(comment
  (require '[erv-fib-synth.synths :as synths])
  (midi-event :note-on (fn [msg] (synths/low)))
  (osc/osc-close (recv :server))
  (midi-event :note-on println :note-off println ))
(comment (osc/osc-rm-all-listeners server))
