(ns tieminos.midi.algo-note
  (:require
   [overtone.music.time :refer [apply-at]]
   [tieminos.midi.mpe :refer [mpe-note-off mpe-note-on]]
   [time-time.standard :refer [now]]))

(comment
  "Usage"
  (require '[erv-fib-synth.midi.core :refer [midi-in-event]]
           '[erv.cps.core :as cps])
  (def outy (midi/midi-out "VirMIDI"))
  (def hex (cps/make 2 [1 3 5 7]))

  ;; programmatically send note-on and note-off based on `dur`
  (algo-note :sink outy
             :dur 1000;; milliseconds
             :scale (:scale hex)
             :base-freq 440
             :get-pitch-class get-cps-pitch-class
             :deg-offset 0
             :midi-note 4
             :vel 60))

(def algonote-messages (atom []))
(comment
  (->> @algonote-messages
       (group-by second)
       (map (fn [[n msgs]] (let [g (group-by first msgs)] [n (count (:on g))
                                                          (count (:off g))])))))
(defn schedule-note-off
  ([sink after-ms note]
   (swap! algonote-messages conj [:off note])
   (apply-at (+ after-ms (now))
             #(mpe-note-off sink note))))

(defn- delay-fn [time-ms f]
  (if (zero? time-ms)
    (f)
    (apply-at (+ (now) time-ms) f)))

(defn- algo-note*
  [& {:keys [sink
             scale
             base-freq
             get-pitch-class
             deg-offset
             midi-note
             dur
             vel
             delay]
      :or {deg-offset 0
           get-pitch-class (fn [_ degree] degree)
           base-freq 440
           delay 0}}]
  (delay-fn delay
            #(let [note-id (java.util.UUID/randomUUID)]
               (mpe-note-on :sink sink
                            :scale scale
                            :base-freq base-freq
                            :get-pitch-class get-pitch-class
                            :deg-offset deg-offset
                            :midi-note midi-note
                            :vel vel
                            :note-id note-id)
               (swap! algonote-messages conj [:on midi-note])
               (schedule-note-off sink dur note-id))))

(defn- chord? [midi-note]
  (or (vector? midi-note)
      (list? midi-note)))

(do
  (defn- proportional-extension [size numbers]
    (let [numbers* (cond (number? numbers) [numbers]
                         (or (list? numbers) (vector? numbers)) numbers
                         :else [1000])
          proportion (Math/ceil (float (/ size (count numbers*))))]
      (->> numbers*
           (map #(repeat proportion %))
           (apply concat)
           (take size))))
  (proportional-extension 5 [1 2 3]))

(defn ensure-number
  "If `x` is a nuber, return it, else if a seq is received (assuming a list of numbers), return the the first value"
  [x]
  (if (number? x) x (first x)))

(defn algo-note
  "Will send a `note-on` and a `note-off` `msg` after `dur`.
  Use for algorithmic/programmed music.
  `midi-note` can be a single midi note number of a vector/list of midi note numbers"
  [& {:keys [sink scale base-freq get-pitch-class deg-offset midi-note dur vel delay]
      :or {deg-offset 0
           get-pitch-class (fn [_ degree] degree)
           base-freq 440
           delay 0}}]
  (if (chord? midi-note)
    (doseq [[midi-note* dur* vel* delay*]
            (map (fn [& args] args)
                 midi-note
                 (proportional-extension (count midi-note) dur)
                 (proportional-extension (count midi-note) vel)
                 (proportional-extension (count midi-note) delay))]
      (algo-note* :sink sink
                  :dur dur*
                  :scale scale
                  :base-freq base-freq
                  :get-pitch-class get-pitch-class
                  :deg-offset deg-offset
                  :midi-note midi-note*
                  :vel vel*
                  :delay delay*))
    (algo-note* :sink sink
                :dur (ensure-number dur)
                :scale scale
                :base-freq base-freq
                :get-pitch-class get-pitch-class
                :deg-offset deg-offset
                :midi-note midi-note
                :vel (ensure-number vel)
                :delay (ensure-number delay)))
 true)
