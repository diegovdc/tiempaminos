(ns tieminos.habitat.main-sequencer
  (:require
   [tieminos.utils :refer [seconds->dur]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(def bpm 60)

(defn sequencer [sections & {:keys [starting-section-index] :or {starting-section-index 0}}]
  (let [sections*   (map #(update-in % [0] seconds->dur bpm)
                         sections)
        durs (map first sections*)]
    (ref-rain :id ::main
              :durs durs
              :loop? false
              :tempo bpm
              :on-event (on-event ((-> sections
                                       (nth (+ starting-section-index index))
                                       second))))))

(defn timestamps->dur-intervals*
  "Transforms a vector of shape [[minutes seconds]] to a vector of durations [dur].
  Assumes that the first timestamp will always be [0 0].
  Will add a duration of 5 to the last event (this shouldn't matter in practice as it it just to have the ref-rain play that last event)."
  [timestamps]
  (->> timestamps
       (reduce (fn [durs [min sec]]
                 (conj durs (- (+ (* 60 min) sec)
                               (or (last durs) 0))))
               [])
       (drop 1)
       (into [])
       (#(conj % 5))))

(defn timestamps->dur-intervals
  "See docstring for `timestamps->dur-intervals*`."
  [sections]
  (mapv vector
        (timestamps->dur-intervals* (map first sections))
        (map second sections)))

(comment
  (sequencer
   (timestamps->dur-intervals [[[0 0] #(println "hola")]
                               [[0 5] #(println "adios")]
                               [[0 10] #(println ".")]]))

  (timestamps->dur-intervals [[[0 0] #(println "hola")]
                              [[0 5] #(println "adios")]
                              [[0 10] #(println ".")]])

  (timestamps->dur-intervals* [[0 0]
                               [5 30]
                               [6 31]]))
