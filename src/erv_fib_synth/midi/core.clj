(ns erv-fib-synth.midi.core
  (:require [clojure.set :as set]
            [overtone.midi :as midi]
            [overtone.sc.node :refer [ctl node?]]
            [taoensso.timbre :as timbre]))

;; If getting the error Device is Busy:
;; https://linuxmusicians.com/viewtopic.php?p=133696
;; http://www.tedfelix.com/linux/linux-midi.html
;; HOWEVER is MUCH BETTER to use `sudo modprobe snd-virmidi` to
;; enable virmidi

(def ks (try (midi/midi-in "VirMIDI")
             (catch Exception e
               (timbre/warn (str "Could not connect to VirMIDI: " (.getMessage e))))))

(comment
  (require '[erv.cps.core :as cps]
            '[erv.utils.conversions :as conv]
            '[erv.utils.core :as utils]
            '[erv.scale.core :as scale])
  (do (require '[overtone.core :as o])
      #_(o/connect-external-server))
  (o/defsynth siny [freq 440 amp 0.2] (o/out 0 (o/pan2 (* amp (o/sin-osc freq)) 0)))
  (def hex (cps/make 2 [1 3 5 7]))
  (def f1 (-> hex :scale first :bounded-ratio (* 440)))
  (def f4 (-> hex :scale (nth 4) :bounded-ratio (* 440)))
  (-> f4 conv/cps->midi (- 78)  calculate-bend)
  (def s1 (siny :freq f1  :amp 0.1))
  (def s2 (siny :freq f4  :amp 0.1))
  (o/ctl s1 :freq f1)
  (o/clear)
  (do

    (midi->mpe-note 70.5513962033396)
    #_(midi->mpe-note (-> f1 conv/cps->midi)))

  (do

    (println (deg->mpe-note (:scale hex) 440 0)))

  (-> #{1} ((partial set/difference #{1 2})))

  (-> @midi-state)
  (get-available-channel 2)
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

  (do (defn remove-midi-note [midi-state midi-note pitch-class]
        (-> midi-state
            (dissoc midi-note)
            (remove-midi-note-from-pitch-class-list pitch-class midi-note)
            )
        )
      (-> (remove-midi-note @midi-state 0 #{7 5})
          (remove-midi-note 6 #{7 5})))

  (defn get-cps-pitch-class [cps-scale degree]
    (let [note (utils/wrap-at degree cps-scale)]
      (println note)
      (when (not= 2 (:bounding-period note))
        (timbre/warn "CPS pitch does not have a bounding period equal to 2. Midi notes might be out of tune. Please use another method to determine pitch class."))
      (:set note)))
  (get-cps-pitch-class (:scale hex) 0)
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

  (def s1 (siny :freq f1  :amp 0.3))
  (o/ctl s1 :freq 440)
  (def s2 (siny :freq f4  :amp 0.5))
  (def outy (midi/midi-out))
  #_(mpe-note-on outy (:scale hex) 440 0 4 30 0)
  (mpe-note-on :sink outy
               :scale (:scale hex)
               :base-freq 440
               :get-pitch-class get-cps-pitch-class
               :deg-offset 0
               :midi-note 4
               :vel 60)
  (defn mpe-note-off [sink midi-note]
    (let [{:keys [note chan pitch-class]} (@midi-state midi-note)]
      (when note
        (midi/midi-note-off outy note chan)
        (swap! midi-state remove-midi-note midi-note pitch-class))))
  (-> @midi-state)
  (reset! midi-state {})
  (mpe-note-off outy 4)
  (do
    (o/clear)
    (midi/midi-control outy 123 0)
    (doseq [n (range 128)] (midi/midi-note-off outy n))))


(comment
  (midi-event
   :note-on (fn [msg]
              (println "note on" ((juxt :note :velocity) msg))
              (println "note-data"
                       (mpe-note-on :sink outy
                                    :scale (:scale hex)
                                    :base-freq 30
                                    :get-pitch-class get-cps-pitch-class
                                    :deg-offset 10
                                    :midi-note (msg :note)
                                    :vel (msg :velocity))))
   :note-off #(mpe-note-off outy (% :note)))
  )

(comment
  (midi/midi-in)
  (midi/midi-devices)
  (midi/midi-sources)
  (midi/midi-sinks))

(defn note-on [f]
  "`f` receives a map with the following keys
   `'(:data2 :command :channel :msg :note :status :data1 :device :timestamp :velocity)`"
  (midi/midi-handle-events
   ks
   (fn [ev]
     (try
       (cond
         (-> ev :command (= :note-on)) (f ev)
         :else nil)
       (catch Exception e (timbre/error "MIDIError" e))))))

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

(defn get-auto-gate-ctl [auto-ctl]
  (if auto-ctl
    {:add add-synth :remove (partial remove-synth ctl)}
    {:add (fn [_ _] nil) :remove (fn [_] nil)}))

(defn- handle-midi-event
  [ev {:keys [note-on note-off auto-ctl]}]
  (try
    (let [cmd (:command ev)
          gate-ctl (get-auto-gate-ctl auto-ctl)]
      (cond
        (and (= cmd :note-on) note-on) ((gate-ctl :add) ev (note-on ev))
        (and (= cmd :note-off) note-off) (do (note-off ev)
                                             ((gate-ctl :remove) ev))
        :else nil))
    (catch Exception e (timbre/error "MIDIError" e))))

(defn midi-event
  "`note` events receive a map with the following keys
   `'(:data2 :command :channel :msg :note :status :data1 :device :timestamp :velocity)`"
  [& {:keys [note-on note-off auto-ctl]
      :or {auto-ctl true
           note-off (fn [_] nil)}}]
  (midi/midi-handle-events
   ks
   (fn [ev] (handle-midi-event ev
                              {:note-on note-on
                               :note-off note-off
                               :auto-ctl auto-ctl}))))


(comment
  (note-on (fn [_] (println (:note _))))
  (midi-event :note-on (fn [_] (println "on"))
              :note-off (fn [_] (println "off")) )
  )
