(ns tieminos.osc.mpe
  "Using quite a bit of the `erv-fib-synth.midi.mpe` implementation"
  (:require
   [overtone.music.time :refer [apply-at]]
   [taoensso.timbre :as timbre]
   [tieminos.midi.mpe :as midi-mpe]
   [tieminos.osc.core :as osc]
   [time-time.standard :refer [now]]))

(def get-cps-pitch-class midi-mpe/get-cps-pitch-class)

(defn mpe-note-on
  [& {:keys [scale base-freq get-pitch-class deg-offset midi-note vel dur]
      :or {deg-offset 0 get-pitch-class (fn [_ degree] degree)}}]
  (let [degree (+ midi-note deg-offset)
        pitch-class (get-pitch-class scale degree)
        chan (midi-mpe/get-available-channel pitch-class)
        {:keys [note bend] :as data} (midi-mpe/deg->mpe-note scale
                                                             base-freq
                                                             degree)
        note-data (assoc data
                         :input-midi-note midi-note
                         :vel vel
                         :chan chan
                         :pitch-class pitch-class)]
    (println note-data)
    (if (> note 127)
      (do (timbre/error
           "Calculated MIDI note out of range. Change :deg-offset or the :base-freq to get the output :note into range: "
           (select-keys note-data [:input-midi-note :note]))
          note-data)
      (do
        (if dur
          (osc/send-note* :dur dur :note note :bend bend :vel vel :chan chan)
          (osc/send-note-on :note note :vel vel :bend bend :chan chan))
        (swap! midi-mpe/midi-state
               midi-mpe/add-midi-note
               "osc"
               midi-note
               note-data
               pitch-class)
        note-data))))

(defn mpe-note-off
  [midi-note]
  (let [{:keys [note chan pitch-class]} (@midi-mpe/midi-state midi-note)]
    (when note
      (osc/send-note-off :note note :chan chan)
      (swap! midi-mpe/midi-state
             midi-mpe/remove-midi-note
             midi-note pitch-class))))
