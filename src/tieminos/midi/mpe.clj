(ns tieminos.midi.mpe
  (:require [clojure.set :as set]
            [erv.scale.core :as scale]
            [erv.utils.conversions :as conv]
            [erv.utils.core :as utils]
            [overtone.midi :as midi]
            [taoensso.timbre :as timbre]))

(comment
  "Usage"
  (require '[erv-fib-synth.midi.core :refer [midi-in-event]]
           '[erv.cps.core :as cps])
  (def outy (midi/midi-out "VirMIDI"))
  (def hex (cps/make 2 [1 3 5 7]))

  ;;  While receiving input from a midi keyboard
  (midi-in-event
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

(comment
  "Testing for synth mpe capabilities"
  (require '[overtone.core :as o])
  (o/sin))

(declare get-available-channel deg-mpe-note add-midi-note remove-midi-note deg->mpe-note)

(def config
  "`:bend-range` can be :8-bit (java interface) `:14-bit` (if using the osc to send to supercollider or something)"
  (atom {:bend-range :8-bit}))

;; "It's a map of channel numbers to note-data, and will also contain a
;; `:pitch-class` key that is a map of pitch-class to midi-channel
;; (used to optimize the number of notes that MPE provides)"
(defonce midi-state (atom {}))

(defn mpe-note-on
  "Sends a note on message with pitch bend data
  `note-id` can be used to have a unique id in the midi-state atom. useful when sending overlapping midi note-on messages for the same note."
  [& {:keys [sink scale base-freq get-pitch-class deg-offset midi-note vel note-id]
      :or {deg-offset 0
           get-pitch-class (fn [_ degree] degree)
           base-freq 440}}]
  (let [degree (+ midi-note deg-offset)
        pitch-class (get-pitch-class scale degree)
        chan (get-available-channel sink pitch-class)
        {:keys [note bend] :as data} (deg->mpe-note scale
                                                    base-freq
                                                    degree)
        note-data (assoc data
                         :sink sink
                         :input-midi-note midi-note
                         :vel vel
                         :chan chan
                         :pitch-class pitch-class)]
    (if (> note 127)
      (do (timbre/error
           "Calculated MIDI note out of range. Change :deg-offset or the :base-freq to get the output :note into range: "
           (select-keys note-data [:input-midi-note :note]))
          note-data)
      (when chan
        (timbre/debug "note " note)
        (midi/midi-pitch-bend sink bend chan)
        (midi/midi-note-on sink note vel chan)
        (swap! midi-state add-midi-note
               sink
               (or note-id midi-note)
               note-data
               pitch-class)
        note-data))))

(comment
  (def outy (midi/midi-out "VirMIDI"))
  (midi/midi-note-on outy 1 60 1)
  (reset! midi-state {})
  (-> @midi-state))

(defn mpe-note-off [sink note-id]
  (let [{:keys [note chan pitch-class input-midi-note]} (get-in @midi-state [sink note-id])]
    (when note
      (timbre/debug "note off" note)
      (midi/midi-note-off sink note chan)
      (swap! midi-state
             remove-midi-note
             sink
             note-id
             input-midi-note
             pitch-class))))

(comment
  (algo-note :sink outy
             :dur [4000 7000] ;; milliseconds
             :scale (:scale hex)
             :base-freq 440
             :get-pitch-class get-cps-pitch-class
             :deg-offset -6
             :midi-note [4 5]
             :delay [0 500]
             :vel [20 40])

  (add-watch midi-state nil (fn [_ _ _ s] (println s)))
  (-> @midi-state)
  (reset! midi-state {})
  (all-notes-off outy))

(defn calculate-bend
  "Does an upward bend to a note. Has a resolution of ≈ 100/63 cents"
  [midi-decimals]
  ;; 64 is assumed to be the central note in an 8-bit pitch bend (at least for Reaper)
  (let [bend-center (case (@config :bend-range)
                      :14-bit 8192
                      :8-bit 64)]
    (Math/round (+ bend-center (* midi-decimals (dec bend-center))))))

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

(defn- get-oldest-available-used-channel-or-new
  [chan-history available-channels]
  (let [history (reduce (fn [{:keys [set* list*] :as acc} el]
                          (if (or (set* el)
                                  (nil? (available-channels el)))
                            acc
                            {:set* (conj set* el)
                             :list* (conj list* el)}))
                        {:set* #{} :list* ()}
                        chan-history)]
    (if (= available-channels (:set* history))
      ;; when all available channels have been used
      (first (history :list*))
      ;; When some available channels have not been used
      (rand-nth (vec (set/difference available-channels (:set* history)))))))

(comment
  (get-oldest-available-used-channel-or-new '(1 2 1 2 3 1 2 1 4 2 1) #{3 4 5})
  (get-oldest-available-used-channel-or-new '(1 2 5 1 2 3 1 2 1 4 2 1) #{3 4 5}))

(defn- get-previously-used-channel-or-new
  "If the previously used channel is available, use that, else, use a new one"
  [prev-pitch-class-chan chan-history available-channels]
  (or (available-channels prev-pitch-class-chan)
      (get-oldest-available-used-channel-or-new
       chan-history available-channels)
      (first available-channels)))

(comment
  (get-previously-used-channel-or-new 1 '(1 2 1 2 3) #{4}))

(defn get-available-channel [sink pitch-class]
  (let [state (get @midi-state sink {})
        pitch-classes (state :pitch-classes {})
        pitch-class-chan  (:chan (pitch-classes pitch-class))
        prev-pitch-class-chan (get-in
                               state
                               [:previous-pitch-classes :map pitch-class])
        chan-history (get-in state [:previous-pitch-classes :chan-history])
        size (count pitch-classes)]
    (cond
      ;; channel in use for pitch-class
      pitch-class-chan pitch-class-chan
      ;; all channels used
      (= 15 size) nil
      ;; some channels available
      :else (->> (state :pitch-classes)
                 vals
                 (map :chan)
                 set
                 (set/difference midi-channels)
                 #_first
                 (get-previously-used-channel-or-new
                  prev-pitch-class-chan
                  chan-history)))))

(defn add-midi-note [midi-state sink midi-note note-data pitch-class]
  (let [state (assoc-in midi-state [sink midi-note] note-data)]
    (when pitch-class
      (-> state
          (assoc-in [sink :pitch-classes pitch-class :chan] (:chan note-data))
          (update-in [sink :pitch-classes pitch-class :midi-notes] conj
                     (:input-midi-note note-data))))))

#_(add-midi-note
   {4 {:note 66, :bend 107, :input-midi-note 4, :vel 20, :chan 8, :pitch-class #{7 1}}, :pitch-classes {#{7 1} {:chan 8, :midi-notes '(4)}}}
   "abcd"
   {:note-data 1}
   #{:pitch-class})

(defn- add-prev-used-chan
  "Will add `pitch-class` and `last-chan` to `prev-pitch-classes-map`.
  If the value of `last-chan` is associated with another pitch-class then
  this pitch-class will be ignored in favor of the provided `pitch-class`"
  [prev-pitch-classes-map pitch-class last-chan]
  (reduce
   (fn [acc [pc chan]]
     (if-not (or (= chan last-chan)
                 (= pc pitch-class))
       (assoc acc pc chan)
       acc))
   {pitch-class last-chan}
   prev-pitch-classes-map))

(comment
  ;; remove :c add :d 3
  (add-prev-used-chan {:a 1 :b 2 :c 3} :d 3)
  ;; :d will be 3
  (add-prev-used-chan {:a 1 :b 2 :d 5} :d 3))

(do
  (defn remove-first-note [note coll]
    (let [idx (.indexOf coll note)]
      (into (vec (take idx coll))
            (drop (inc idx) coll))))

  (remove-first-note #{1} '(0 2 #{1} 1)))

(defn remove-midi-note-from-pitch-class-list [midi-state sink pitch-class midi-note]
  (let [state (update-in midi-state
                         [sink :pitch-classes pitch-class :midi-notes]
                         #(remove-first-note midi-note %))]
    (if (-> state :pitch-classes (get pitch-class) :midi-notes seq)
      state
      (let [last-chan (-> state :pitch-classes (get pitch-class) :chan)]
        (-> state
            (update-in [sink :pitch-classes] dissoc pitch-class)
            (update-in [sink :previous-pitch-classes :map]
                       add-prev-used-chan  pitch-class last-chan)
            (update-in [sink :previous-pitch-classes :chan-history]
                       #(conj (take 200 %) last-chan)))))))

(defn remove-midi-note
  ([midi-state sink note-id input-midi-note pitch-class]
   (-> midi-state
       (update sink dissoc note-id)
       (remove-midi-note-from-pitch-class-list sink pitch-class input-midi-note))))

(defn get-cps-pitch-class [cps-scale degree]
  (let [note (utils/wrap-at degree cps-scale)]
    (when (not= 2 (:bounding-period note))
      (timbre/warn "CPS pitch does not have a bounding period equal to 2. Midi notes might be out of tune. Please use another method to determine pitch class."))
    (:set note)))

(comment (get-cps-pitch-class (:scale hex) 0))

(defn all-notes-off [sink] (midi/midi-control sink 123 0))

(comment
  (def outy (midi/midi-out))

  (-> @midi-state)
  (reset! midi-state {})
  (mpe-note-off outy 4)
  (all-notes-off outy)
  (-> outy)
  ;; all notes off
  (midi/midi-note-on outy 60 60 1)
  (midi/midi-note-on outy 69 60 2)
  (midi/midi-pitch-bend outy 63 0)
  (midi/midi-pitch-bend outy 28 2)
  (midi/midi-pitch-bend outy 64 2)
  (midi/midi-pitch-bend outy 127 2)
  #_(doseq [ch (range 127)]
      (midi/midi-control outy ch 63))
  (midi/midi-note-off outy 69 2))
