(ns tieminos.compositions.garden-earth.synths.recording-v2
  (:require
   [tieminos.compositions.garden-earth.base
    :refer [eik-notes pitch-class->pr-fingering stop]]
   [tieminos.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [tieminos.compositions.garden-earth.synths.granular :refer [grain]]
   [tieminos.sc-utils.recording.v1 :as sc.rec.v1]))


(defn replay [buf-key & {:keys [speed]
                         :or {speed 1}}]
  (let [b (@sc.rec.v1/bufs buf-key)]
    (grain b
           (/ (:duration b) speed)
           :speed speed
           :trig-rate 40
           :grain-dur 1/20
           :pos-noise-amp 0
           :amp 3)))

(defn get-any-buffer-for-pitch-class
  ([pc] (get-any-buffer-for-pitch-class pc @sc.rec.v1/bufs))
  ([pc bufs] (->> bufs
                  (filter (fn [[[k]]] (= k pc)))
                  rand-nth
                  second)))
(comment
  (get-any-buffer-for-pitch-class "A+53" @sc.rec.v1/bufs))

(defn rec-with-fingering
  "Plays a reference pitch using `tuning-monitor`
  `buf-key` should be [pitch-class-string & ids] "
  [{:keys [in seconds buf-key on-end]
    :or {in 0}}]
  (let [pitch-class (first buf-key)]
    (when (> 10 seconds)
      (tuning-monitor (-> eik-notes (get pitch-class) :bounded-ratio (* 440))
                      seconds
                      seconds))
    (tuning-monitor (-> eik-notes (get pitch-class) :bounded-ratio (* 440))
                    10
                    seconds)
    (sc.rec.v1/start-recording
     :bufs-atom sc.rec.v1/bufs
     :buf-key buf-key
     :seconds seconds
     :input-bus in
     :msg (format "\nWill record %s for %s seconds\nFingering:\n%s \n\n"
                  pitch-class
                  seconds
                  (pitch-class->pr-fingering pitch-class))
     :on-end on-end)))

(comment
  (sc.rec.v1/rec-buf {:bufs-atom sc.rec.v1/bufs
                      :buf-key [:oli 5 6]
                      :seconds 5})
  (replay ["A+53" :a])
  (stop)
  (sc.rec.v1/start-recording
   :bufs-atom sc.rec.v1/bufs
   :buf-key :olips
   :seconds 5
   :msg "Will record G#5+45"
   :on-end replay))
