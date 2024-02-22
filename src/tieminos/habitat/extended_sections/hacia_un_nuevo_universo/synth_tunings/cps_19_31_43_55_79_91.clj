(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.synth-tunings.cps-19-31-43-55-79-91
  (:require
   [erv.constant-structures.graphics :refer [init-cs-tool! update-state]]
   [erv.cps.core :as cps]
   [erv.cps.utils :as cps-utils]
   [erv.scale.core :refer [deg->freq]]
   [tieminos.harmonic-experience.drones.sounds :refer [harmonic]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]))

(def eikosany
  (->> (cps/make 3 [19 31 43 55 79 91])
       (cps/+all-subcps)))

(def hexanies
  (->> eikosany
       :subcps
       (filter (fn [[_ {:keys [scale]}]] (= 6 (count scale))))
       (into {})))

(comment
  (cps-utils/harmonic-triad-degrees
   (second (nth (->> hexanies
                     (into [])
                     (sort-by first))
                @current-key-index))
   0))

(comment

  (def cs-tool (init-cs-tool! [] []))
  (swap! cs-tool assoc :max-arcs 10)

  (def current-key-index (atom 0))

  (def oxygen (get-oxygen!))

  (swap! current-key-index inc)
  (let [index @current-key-index
        hexany (nth (->> hexanies
                         (into [])
                         (sort-by first))
                    index)
        scale (-> hexany second :scale)]

    (println (first hexany))
    #_(swap! current-key-index inc)
    (update-state cs-tool scale [])

    (when oxygen
      (midi-in-event
       :midi-input oxygen
       :note-on (fn [ev]
                  (harmonic :freq (deg->freq scale
                                             112
                                             (- (:note ev) 40))
                            :a (linexp* 0 127 4 0.5 (:velocity ev))
                            :r 3
                            :amp (linexp* 0 127 0.3 2 (:velocity ev))))))))
