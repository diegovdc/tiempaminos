(ns tieminos.midi.scl
  "Midi functions for scl style synths"
  (:require
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]))

(comment
  (declare scl-note-off scl-note-on)
  (def outy (midi/midi-out "VirMIDI"))
  (scl-note-on {:sink outy :midi-note 60})
  (scl-note-off outy 60)
  (require '[tieminos.midi.algo-note :refer [algo-note]])
  (algo-note :sink outy
             :dur 1000 ;; milliseconds
             :midi-note 80
             :vel 60
             :note-on-fn scl-note-on
             :note-off-fn scl-note-off))

(defn get-chan-and-note [base-chan input-midi-note]
  (let [degree (+ input-midi-note)
        chan (+ base-chan (quot degree 127))
        note (mod degree 127)]
    {:chan chan :note note}))

(defn scl-note-on
  "Send `note-on` message for synths that use scl scales. Can accept `midi-note`s greater than 127 in which case it will choose the next channel"
  [& {:keys [sink midi-note vel base-chan]
      :or {base-freq 440
           base-chan 0
           vel 64}}]
  (let [{:keys [chan note]} (get-chan-and-note base-chan
                                               midi-note)
        note-data {:input-midi-note midi-note :chan chan :vel vel}]
    (println chan)
    (timbre/debug "note on" note)
    (midi/midi-note-on sink note vel chan)
    note-data))

(defn scl-note-off
  ([sink midi-note] (scl-note-off 0 sink midi-note))
  ([base-chan sink midi-note]
   (let [{:keys [chan note]} (get-chan-and-note base-chan
                                                midi-note)]
     (timbre/debug "note off" note)
     (midi/midi-note-off sink note chan))))
