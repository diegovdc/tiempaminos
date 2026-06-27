(ns tieminos.midi.core
  (:require
   [overtone.core :as o]
   [overtone.libs.event :as overtone.event]
   [overtone.midi :as midi]
   [overtone.sc.node :refer [ctl node?]]
   [taoensso.timbre :as timbre]))

(comment
  ;; basic USAGE
  (midi-in-event
   :midi-input (get-oxygen!)
   :note-on (fn [_] (println "pepe")))

  ;; MPE usage (exquis)
  (require
   '[overtone.core :as o]
   '[tieminos.overtone-extensions :as oe]
   '[erv.utils.conversions :as conv]
   '[tieminos.math.utils :refer [linlin* linexp*]]
   '[tieminos.synths :refer [demo-sine]])

  (midi-in-event
   :midi-input (get-exquis!)
   :note-on (fn [{:keys [note velocity]}]
              (demo-sine :freq (conv/midi->cps
                                (+ (exquis-cc 41) ;; will produce a brief glissando on the attack
                                   note))
                         :amp (linlin* 0 127 0.4 0.9 velocity)))
   :keep-cc-state? {:log? true}
   :cc (fn [{:keys [note velocity]}]
          ;; for some reason javax.sound.midi misses some note-off messages so...
         (when (and (= note 21) ;; knob 1 click
                    (= velocity 127))
           (clear-all-synths!)))
   :before-gate-0 (fn [{:keys [velocity]} synth]
                    (o/ctl synth :r (linlin* 0 127 3 1 velocity)))
   :mpe {:x (fn [synth val]
              (let [note (::midi-note synth)]
                (o/ctl synth :freq (-> (linlin* 0 127 (- note 2) (+ note 2) val)
                                       (conv/midi->cps)))))
         :y (fn [synth val]
              (o/ctl synth :pan (linlin* 0 127 -1 1 val)))
         :z (fn [synth val]
              (o/ctl synth :amp (linexp* 0 127 0.1 1 val)))}))

;; If getting the error Device is Busy:
;; https://linuxmusicians.com/viewtopic.php?p=133696
;; http://www.tedfelix.com/linux/linux-midi.html
;; HOWEVER is MUCH BETTER to use `sudo modprobe snd-virmidi` to
;; enable virmidi

;; NOTE: on virmidi, if overtone starts to crash on load, maybe virmidi was corrupted (has already happend to me once)
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
(defonce exquis* (atom nil))

(defn get-exquis!
  "MPE z-axis or pressure is `:channel-brightness`, and y-axis sound brightness is cc 74"
  []
  (if @exquis*
    @exquis*
    (let [ctlr (-> (midi/midi-in "Exquis")
                   (assoc ::state (atom {})))]
      (try (reset! exquis* ctlr)
           (catch Exception e
             (timbre/warn (str "Could not connect to Exquis: " (.getMessage e))))))))

(defn get-cc
  ([midi-input-state-data cc] (get-cc midi-input-state-data cc 0))
  ([midi-input-state-data cc chan] (get-in midi-input-state-data [chan cc] 0)))

(defn exquis-cc
  "chans 21-23 (press) and 41-44 (rotate)"
  ([cc] (exquis-cc cc 0))
  ([cc chan]
   (get-cc (deref (::state @exquis*)) cc chan)))

(defonce pacer* (atom nil))

(defn get-pacer!
  []
  (if @pacer*
    @pacer*
    (try (reset! pacer* (midi/midi-in "PACER MIDI1"))
         (catch Exception e
           (timbre/warn (str "Could not connect to Pacer: " (.getMessage e)))))))

(defonce iac2* (atom nil))

(defn get-iac2!
  " iac1 is used for sending and iac2 for receiving
  Internal mac routing aka VirMIDI"
  []
  (if @iac2*
    @iac2*
    (try (reset! iac2* (midi/midi-in "Bus 2"))
         (catch Exception e
           (timbre/warn (str "Could not connect to iac2: " (.getMessage e)))))))

(comment
  (reset! iac2* nil)
  (midi/midi-out "VirMIDI")
  (midi/midi-in)
  (midi/midi-devices)
  (midi/midi-sources)
  (midi/midi-sinks))

(defn note-on
  "`f` receives a map with the following keys
   `'(:data2 :command :channel :msg :note :status :data1 :device :timestamp :velocity)`"
  ([midi-input f]
   (midi/midi-handle-events
    midi-input
    (fn [ev]
      (try
        (cond
          (-> ev :command (= :note-on)) (f ev)
          :else nil)
        (catch Exception e (timbre/error "MIDIError" e)))))))

(defonce synths (atom {}))

(defn- synth-midi-data [{:keys [channel note velocity]} synth-or-synths]
  (if (sequential? synth-or-synths)
    (map #(assoc %
                 ::midi-chan channel
                 ::midi-note note
                 ::midi-velocity velocity)
         synth-or-synths)
    (assoc synth-or-synths
           ::midi-chan channel
           ::midi-note note
           ::midi-velocity velocity)))

(defonce events (atom []))
(comment
  (reset! events []))
(defn log-ev
  [type note]
  (swap! events conj [type note]))

(defn add-synth [{:keys [note] :as ev} synth]
  (when (or (node? synth)
            (and (seq synth) (every? node? synth)))
    (log-ev :add note)
    (swap! synths update note
           (fnil conj [])
           (synth-midi-data ev synth))))

(comment
  (o/stop)
  (clear-all-synths!)
  (sequential? [{:i-am :synth} {:i-am :synth2}])
  (with-redefs [synths (atom {})
                node? map?]
    (add-synth {:note 5} [{:i-am :synth}
                          {:i-am :synth2}]))
  (add-synth {:note 5} [{:i-am :synth}
                        {:i-am :synth2}])
  (-> @synths)
  (reset! synths {}))

(comment
  (require '[tieminos.overtone-extensions :as oe])
  (oe/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc 200)))))

  (def test-sini
    (-> (sini)  (assoc :midi-chan 1)))
  (seq? test-sini)
  (o/kill test-sini))

(comment
  (-> @synths)
  (-> @events)
  (clear-all-synths!)
  (reset! events []))

(defn remove-synth
  ([ctl ev] (remove-synth ctl ev nil))
  ([ctl ev before-gate-0]
   (let [note-synths (@synths (:note ev))
         synths-to-kill (first note-synths)
         live-synths (into [] (rest note-synths))
         kill-fn (fn [ev synth]
                   (when (fn? before-gate-0) (before-gate-0 ev synth))
                   (ctl synth :gate 0))]
     (log-ev :remove (:note ev))
     (cond
       (node? synths-to-kill) (kill-fn ev synths-to-kill)

       (and (seq synths-to-kill) (every? node? synths-to-kill))
       (doseq [s synths-to-kill] (kill-fn ev s)))

     (if (seq live-synths)
       (swap! synths assoc (:note ev) live-synths)
       (swap! synths dissoc (:note ev))))))

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
(->> @synths
     vals
     flatten
     (map ::midi-chan))

(do (defn- get-chan-synths [chan]
      (->> @synths
           vals
           flatten
           (filter #(= (::midi-chan %) chan))))
    #_(get-chan-synths 10))

#_(doseq [sy (get-chan-synths 9)]
    (o/ctl sy :amp 0.7))

(defonce round-robin-state (atom {}))

(defn- handle-midi-event
  "`dup-note-mode` #{:multi :round-robin} - what to do whane multiple consecutive note-on events happen on a single midi note (without alternating note-off events)
    - `:multi` allow any number of synths to be triggered on a single note
    - `:round-robin` allow only one note at a time, killing the previous synth playing on that note"
  [ev {:keys [note-on note-off cc pitch-bend channel-pressure auto-ctl? before-gate-0 dup-note-mode]
       :as params
       :or {dup-note-mode :multi}}]
  (try
    (let [cmd (:command ev)
          gate-ctl (get-auto-gate-ctl auto-ctl?)]

      (cond
        (= cmd :control-change) (cc ev)
        (= cmd :pitch-bend) (pitch-bend ev)
        (= cmd :channel-pressure) (channel-pressure ev)

        (#{:note-on :note-off} cmd)
        (condp = [auto-ctl? cmd dup-note-mode]
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
          [true :note-off :multi] (do (note-off ev) ((gate-ctl :remove) ev before-gate-0))
          [true :note-off :round-robin] (let [note (:note ev)]
                                          (note-off ev)
                                          (when (= 1 (get @round-robin-state [:held-keys note]))
                                            ((gate-ctl :remove) ev))
                                          (swap! round-robin-state update-in [:held-keys note] dec))
          ;; TODO: add tests
          [false :note-on :multi] (note-on ev)
          [false :note-on :round-robin] (note-on ev)
          [false :note-off :multi] (note-off ev)
          [false :note-off :round-robin] (note-off ev))))
    (catch Exception e (timbre/error "MIDIError" e {:ev ev :params params}))))

(declare clear-synths-on-overtone-stop!)

(defn- mpe-fn
  ([f] (mpe-fn f false))
  ([f z?]
   (when (fn? f)
     (fn [{:keys [note channel velocity] :as _ev}]
       (let [synths (get-chan-synths channel)]
         (doseq [sy synths]
           (f sy (if z? note velocity))))))))
(def ^:private y-cc 74)

(defn- wrap-mpe
  [{:keys [x y z] :as _mpe}
   pitch-bend channel-pressure cc]
  {:pitch-bend (or (mpe-fn x) pitch-bend (fn [_] nil))
   :channel-pressure (or (mpe-fn z true) channel-pressure (fn [_] nil))
   :cc (if-not y cc
               (let [y* (mpe-fn y)
                     cc* (or cc (fn [_] nil))]
                 (fn [{:keys [note] :as ev}]
                   (if (= y-cc note)
                     (y* ev)
                     (cc* ev)))))})

(defn- wrap-cc-to-keep-state
  [cc-fn midi-input log? mpe-y?]
  (let [state (::state midi-input)]
    (if-not state
      (do
        (timbre/warn "`midi-input` has no `::state`")
        cc-fn)
      (fn [{:keys [note channel velocity] :as ev}]
        (when-not (and mpe-y? (= y-cc note))
          (when log?
            (println (format "cc%s@%s: %s" note channel velocity)))
          (swap! state assoc-in [channel note] velocity))
        (cc-fn ev)))))

(defn midi-in-event
  "`note` events receive a map with the following keys
   `'(:data2 :command :channel :msg :note :status :data1 :device :timestamp :velocity)`"
  [& {:keys [midi-input note-on note-off pitch-bend cc channel-pressure auto-ctl?
             mpe
             before-gate-0
             keep-cc-state?]
      :or   {auto-ctl?  true
             note-off   (fn [_] nil)
             pitch-bend (fn [_] nil)
             channel-pressure (fn [_] nil)
             cc (fn [_] nil)
             before-gate-0 (fn [_ev _synth] nil)
             mpe {:x nil :y nil :z nil}}}]
  (clear-synths-on-overtone-stop!) ;; handle o/stop event
  ;; if mpe config, wrap the other handelers with the functions from that config
  (let [handlers (cond-> (wrap-mpe mpe pitch-bend channel-pressure cc)
                   keep-cc-state? (update :cc
                                          wrap-cc-to-keep-state
                                          midi-input
                                          (:log? keep-cc-state?)
                                          (:y mpe)))
        handlers* (merge {:before-gate-0 before-gate-0
                          :note-on    note-on
                          :note-off   note-off
                          :auto-ctl?  auto-ctl?}
                         handlers)]
    (midi/midi-handle-events
     midi-input
     (fn [ev] (handle-midi-event ev handlers*)))))

(defn clear-all-synths!
  []
  (doseq [synth (vals @synths)]
    (try (o/ctl synth :gate 0)
           ;; in case the synth has already been destroyed
         (catch Exception _ nil)))
  (reset! synths {}))

(defn clear-synths-on-overtone-stop!
  []
  (overtone.event/on-event
   :reset
   (fn [& _args]
     (when (seq @synths)
       (timbre/info "Clearing all synths that might have been triggered via a midi input.")
       (clear-all-synths!)))
   ::clear-all-synths))

(defn all-notes-off [sink]
  (doseq [n (range 128) chan (range 16)]
    (midi/midi-note-off sink n chan)))

(comment
  (clear-all-synths!)
  (o/stop)
  (note-on (fn [_] (println (:note _))))
  (all-notes-off (get-oxygen!))
  (midi-in-event :note-on (fn [_] (println "on" ((juxt :channel :note) _)))
                 :note-off (fn [_] (println "off" ((juxt :channel :note) _)))))
