(ns tieminos.habitat.main-sequencer
  (:require
   [taoensso.timbre :as timbre]
   [tieminos.habitat.init :refer [habitat-initialized?]]
   [tieminos.utils :refer [seconds->dur]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(declare timestamps->dur-intervals)

(def bpm 60)

(defonce context (atom {}))

(defn sequencer
  "Sections are a vector of [timestamp side-fx-fn]
  A `timestamp` is [minute second]
  The `side-fx-fn` receives the `context` atom
  `initial-context-data` should include `inputs` `preouts` and `main-fx`"
  [initial-context-data
   sections
   & {:keys [starting-section-index]
      :or {starting-section-index 0}}]
  (when-not @habitat-initialized?
    (throw (ex-info "Must call `tieminos.habitat.init/init!` first" {})))
  (reset! context initial-context-data)
  (let [sections*   (->> sections
                         timestamps->dur-intervals
                         (map #(update-in % [0] seconds->dur bpm)))
        durs (map first sections*)]
    (timbre/info "Starting Habitat")
    (ref-rain :id ::main
              :durs durs
              :loop? false
              :tempo bpm
              :on-event (on-event
                         (let [section-index (+ starting-section-index index)
                               section (-> sections
                                           (nth section-index)
                                           second)]
                           (swap! context assoc
                                  :dur-s dur-s
                                  :section-index section-index)
                           (section context))))))

(do
  (defn timestamps->dur-intervals*
    "Transforms a vector of shape [[minutes seconds]] to a vector of durations [dur].
  Assumes that the first timestamp will always be [0 0].
  Will add a duration of 5 to the last event (this shouldn't matter in practice as it it just to have the ref-rain play that last event)."
    [timestamps]
    (->> timestamps
         (reduce (fn [durs [min sec]]
                   (conj durs (- (+ (* 60 min) sec)
                                 (apply + durs))))
                 [])
         (drop 1)
         (into [])
         (#(conj % 5.12345))))

  (timestamps->dur-intervals*
   [[0 5]
    [0 10]
    [0 14]
    [0 20]
    [0 25]]))

(defn timestamps->dur-intervals
  "See docstring for `timestamps->dur-intervals*`."
  [sections]
  (mapv vector
        (timestamps->dur-intervals* (map first sections))
        (map second sections)))

(comment
  (sequencer
   (timestamps->dur-intervals
    [[[0 0] #(println "hola")]
     [[0 5] #(println "adios")]
     [[0 10] #(println ".")]]))

  (timestamps->dur-intervals [[[0 0] #(println "hola")]
                              [[0 5] #(println "adios")]
                              [[0 10] #(println ".")]])

  (timestamps->dur-intervals* [[0 0]
                               [5 30]
                               [6 31]]))
