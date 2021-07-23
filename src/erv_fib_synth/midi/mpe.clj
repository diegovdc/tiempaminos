(ns erv-fib-synth.midi.mpe
  (:require
   [clojure.set :as set]
   [overtone.midi :as midi]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [taoensso.timbre :as timbre]))

(comment
  "Usage"
  (require '[erv-fib-synth.midi.core :refer [midi-event]]
            '[erv.cps.core :as cps])
  (def outy (midi/midi-out))
  (def hex (cps/make 2 [1 3 5 7]))
  (midi-event
   :note-on (fn [msg]
              (println "note-data"
                       (mpe-note-on :sink outy
                                    :scale (:scale hex)
                                    :base-freq 30
                                    :get-pitch-class get-cps-pitch-class
                                    :deg-offset 10
                                    :midi-note (msg :note)
                                    :vel (msg :velocity))))
   :note-off #(mpe-note-off outy (% :note))))

(declare get-available-channel deg-mpe-note add-midi-note remove-midi-note)

(def midi-state
  "It's a map of channel numbers to note-data, and will also contain a
  `:pitch-class` key that is a map of pitch-class to midi-channel
  (used to optimize the number of notes that MPE provides)"
  (atom {}))

(defn mpe-note-on
  [& {:keys [sink scale base-freq get-pitch-class deg-offset midi-note vel]
      :or {deg-offset 0 get-pitch-class (fn [_ degree] degree)}}]
  (let [degree (+ midi-note deg-offset)
        pitch-class (get-pitch-class scale degree)
        chan (get-available-channel pitch-class)
        {:keys [note bend] :as data} (deg->mpe-note scale
                                                    base-freq
                                                    degree)
        note-data (assoc data
                         :input-midi-note midi-note
                         :vel vel
                         :chan chan
                         :pitch-class pitch-class)]
    (if (> note 127)
      (do (timbre/error
           "Calculated MIDI note out of range. Change :deg-offset or the :base-freq to get the output :note into range: "
           (select-keys note-data [:input-midi-note :note]))
          note-data)
      (do
        (midi/midi-pitch-bend sink bend chan)
        (midi/midi-note-on sink note vel chan)
        (swap! midi-state add-midi-note midi-note note-data pitch-class)
        note-data))))


(defn mpe-note-off [sink midi-note]
  (let [{:keys [note chan pitch-class]} (@midi-state midi-note)]
    (when note
      (midi/midi-note-off sink note chan)
      (swap! midi-state remove-midi-note midi-note pitch-class))))

(defn calculate-bend
  "Does an upward bend to a note. Has a resolution of ≈ 100/63 cents"
  [midi-decimals]
  (println "decimals" midi-decimals)
  ;; 64 is assumed to be the central note in an 8-bit pitch bend (at least for Reaper)
  (Math/round (+ 64 (* midi-decimals 63))))

(defn midi->mpe-note
  [midi]
  (let [midi* (float midi)
        note (int midi*)
        bend (-> (- midi* note) calculate-bend)]
    {:note note :bend bend}))

(defn deg->mpe-note
  [scale fundamental deg]
  (-> (scale/deg->freq scale fundamental deg)
      conv/cps->midi
      midi->mpe-note))

(def midi-channels (set (range 1 16)))

(defn get-available-channel [pitch-class]
  (let [state @midi-state
        pitch-classes (state :pitch-classes {})
        pitch-class-chan  (:chan (pitch-classes pitch-class))
        size (count pitch-classes)]
    (cond pitch-class-chan pitch-class-chan
          (= 15 size) nil
          :else (->> (dissoc state :pitch-classes)
                     vals
                     (map :chan)
                     set
                     (set/difference midi-channels)
                     first))))


(defn add-midi-note [midi-state midi-note note-data pitch-class]
  (let [state (assoc midi-state midi-note note-data)]
    (if pitch-class
      (-> state
          (assoc-in [:pitch-classes pitch-class :chan] (:chan note-data))
          (update-in [:pitch-classes pitch-class :midi-notes] conj (:input-midi-note note-data))))))

(defn remove-midi-note-from-pitch-class-list [midi-state pitch-class midi-note]
  (let [state (update-in midi-state
                         [:pitch-classes pitch-class :midi-notes]
                         #(remove (fn [n] (= n midi-note)) %))]
    (if (-> state :pitch-classes (get pitch-class):midi-notes seq)
      state
      (update state :pitch-classes dissoc pitch-class))))

(defn remove-midi-note [midi-state midi-note pitch-class]
  (-> midi-state
      (dissoc midi-note)
      (remove-midi-note-from-pitch-class-list pitch-class midi-note)))

(comment (-> (remove-midi-note @midi-state 0 #{7 5})
             (remove-midi-note 6 #{7 5})))


(defn get-cps-pitch-class [cps-scale degree]
  (let [note (utils/wrap-at degree cps-scale)]
    (println note)
    (when (not= 2 (:bounding-period note))
      (timbre/warn "CPS pitch does not have a bounding period equal to 2. Midi notes might be out of tune. Please use another method to determine pitch class."))
    (:set note)))

(comment (get-cps-pitch-class (:scale hex) 0))



(comment
  (def outy (midi/midi-out))
  (mpe-note-on :sink outy
               :scale (:scale hex)
               :base-freq 440
               :get-pitch-class get-cps-pitch-class
               :deg-offset 0
               :midi-note 4
               :vel 60)
  (-> @midi-state)
  (reset! midi-state {})
  (mpe-note-off outy 4)

  (-> outy)

  (midi/midi-control outy 123 0) ;; all notes off
  (midi/midi-note-on outy 60 60 1)
  (midi/midi-note-on outy 69 60 2)
  (midi/midi-pitch-bend outy 63 0)
  (midi/midi-pitch-bend outy 28 2)
  (midi/midi-pitch-bend outy 64 2)
  (midi/midi-pitch-bend outy 127 2)
  #_(doseq [ch (range 127)]
      (midi/midi-control outy ch 63))
  (midi/midi-note-off outy 69 2))
