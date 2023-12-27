(ns tieminos.midi.core
  (:require
   [overtone.midi :as midi]
   [overtone.sc.node :refer [ctl node?]]
   [taoensso.timbre :as timbre]))

;; If getting the error Device is Busy:
;; https://linuxmusicians.com/viewtopic.php?p=133696
;; http://www.tedfelix.com/linux/linux-midi.html
;; HOWEVER is MUCH BETTER to use `sudo modprobe snd-virmidi` to
;; enable virmidi

;; NOTE on virmidi, if overtone starts to crash on load, maybe virmidi was corrupted (has already happend to me once)

(defonce ks (try (midi/midi-in "VirMIDI")
                 (catch Exception e
                   (timbre/warn (str "Could not connect to VirMIDI: " (.getMessage e))))))

(defonce oxygen (try (midi/midi-in "USB MIDI")
                     (catch Exception e
                       (timbre/warn (str "Could not connect to USB MIDI: " (.getMessage e))))))

(defonce oxygen* (atom nil))
(defn get-oxygen!
  []
  (if @oxygen*
    @oxygen*
    (try (reset! oxygen* (midi/midi-in "USB MIDI"))
         (catch Exception e
           (timbre/warn (str "Could not connect to USB MIDI: " (.getMessage e)))))))

(defonce lumatone* (atom nil))

(defn get-lumatone!
  []
  (if @lumatone*
    @lumatone*
    (try (reset! lumatone* (midi/midi-in "Lumatone"))
         (catch Exception e
           (timbre/warn (str "Could not connect to Lumatone: " (.getMessage e)))))))

(comment
  ;; basic USAGE
  (midi-in-event
   :midi-input oxygen
   :note-on (fn [_] (println "pepe"))))

(comment
  (midi/midi-out "VirMIDI")
  (midi/midi-in)
  (midi/midi-devices)
  (midi/midi-sources)
  (midi/midi-sinks))

(defn note-on
  "`f` receives a map with the following keys
   `'(:data2 :command :channel :msg :note :status :data1 :device :timestamp :velocity)`"
  ([f] (note-on ks f))
  ([midi-input f]
   (println midi-input f)
   (midi/midi-handle-events
    midi-input
    (fn [ev]
      (try
        (cond
          (-> ev :command (= :note-on)) (f ev)
          :else nil)
        (catch Exception e (timbre/error "MIDIError" e)))))))

(defonce synths (atom {}))

(defn add-synth [ev synth]
  (let [midi-note (:note ev)]
    (when (or (node? synth)
              (and (seq synth) (every? node? synth)))
      (swap! synths update midi-note (fnil conj []) synth))))

(comment
  (with-redefs [synths (atom {})
                node? map?]
    (add-synth {:note 5} [{:i-am :synth}
                          {:i-am :synth2}]))
  (add-synth {:note 5} [{:i-am :synth}
                        {:i-am :synth2}])
  (-> @synths)
  (reset! synths {}))

(defn remove-synth [ctl ev]
  (let [note-synths (@synths (:note ev))
        synths-to-kill (first note-synths)
        live-synths (into [] (rest note-synths))]
    (cond
      (node? synths-to-kill) (ctl synths-to-kill :gate 0)

      (and (seq synths-to-kill) (every? node? synths-to-kill))
      (doseq [s synths-to-kill] (ctl s :gate 0)))

    (if (seq live-synths)
      (swap! synths assoc (:note ev) live-synths)
      (swap! synths dissoc (:note ev)))))

(comment
  (with-redefs [synths (atom {5 [[{:i-am :synth} {:i-am :synth2}]]
                              6 [{:i-am :synth2}]})
                node? map?]
    (remove-synth assoc {:note 5})
    @synths))

(def auto-gate-fns
  {:add add-synth :remove (partial remove-synth ctl)})

(defn get-auto-gate-ctl [auto-ctl]
  (if auto-ctl
    auto-gate-fns
    {:add (fn [_ _] nil) :remove (fn [_] nil)}))

(defn get-note-synths [note]
  (get @synths note))

(defonce round-robin-state (atom {}))

(defn- handle-midi-event
  "`dup-note-mode` #{:multi :round-robin} - what to do whane multiple consecutive note-on events happen on a single midi note (without alternating note-off events)
    - `:multi` allow any number of synths to be triggered on a single note
    - `:round-robin` allow only one note at a time, killing the previous synth playing on that note"
  [ev {:keys [note-on note-off auto-ctl dup-note-mode]
       :or {dup-note-mode :multi}}]
  (try
    (let [cmd (:command ev)
          gate-ctl (get-auto-gate-ctl auto-ctl)]
      (condp = [auto-ctl cmd dup-note-mode]
        [true :note-on :multi] ((gate-ctl :add) ev (note-on ev))
        [true :note-on :round-robin] (let [note (:note ev)
                                           note-synths (get-note-synths note)]
                                       (swap! round-robin-state
                                              assoc-in [:held-keys note]
                                              ;; include the new synth we are playing
                                              (inc (count note-synths)))
                                       (doseq [_ note-synths]
                                         ((gate-ctl :remove) ev))
                                       ((gate-ctl :add) ev (note-on ev)))
        [true :note-off :multi] (do (note-off ev) ((gate-ctl :remove) ev))
        [true :note-off :round-robin] (let [note (:note ev)]
                                        (note-off ev)
                                        (when (= 1 (get @round-robin-state [:held-keys note]))
                                          ((gate-ctl :remove) ev))
                                        (swap! round-robin-state update-in [:held-keys note] dec))))
    (catch Exception e (timbre/error "MIDIError" e))))

(defn midi-in-event
  "`note` events receive a map with the following keys
   `'(:data2 :command :channel :msg :note :status :data1 :device :timestamp :velocity)`"
  [& {:keys [midi-input note-on note-off auto-ctl]
      :or {midi-input oxygen
           auto-ctl true
           note-off (fn [_] nil)}}]
  (midi/midi-handle-events
   midi-input
   (fn [ev] (handle-midi-event ev
                               {:note-on note-on
                                :note-off note-off
                                :auto-ctl auto-ctl}))))

#_(defn all-notes-off [sink] (midi/midi-control sink 123 0))
(defn all-notes-off [sink]
  (doseq [n (range 128) chan (range 16)]
    (midi/midi-note-off sink n chan)))
(comment
  (note-on (fn [_] (println (:note _))))
  (all-notes-off oxygen)
  (midi-in-event :note-on (fn [_] (println "on" ((juxt :channel :note) _)))
                 :note-off (fn [_] (println "off" ((juxt :channel :note) _)))))
