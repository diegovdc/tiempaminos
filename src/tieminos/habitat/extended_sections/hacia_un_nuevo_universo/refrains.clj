(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.refrains
  (:require
   [erv.cps.utils :as cps-utils]
   [erv.scale.core :as scale]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.synth-tunings.cps-19-31-43-55-79-91 :as cps-19-31-43-55-79-91]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

;; TODO WIP
(defn chords1
  [{:keys [id cps chords periods root-freq]
    :or {id ::cps-chords1
         chords [0]
         periods [0]
         root-freq 112}}]
  (let [scale (:scale cps)]
    (ref-rain
      :id id
      :durs [1]
      :on-event (on-event
                  (println (->> (at-i chords)
                                (map #(+ %
                                         (scale/deg->freq scale root-freq %
                                                          {:period (rand-nth periods)} )) )))
                  ))))



(comment
  (gp/stop)
  (chords1
  (let [cps (-> cps-19-31-43-55-79-91/hexanies first second cps-utils/+degrees)]
    {:cps cps
     :chords [(first (cps-utils/harmonic-triad-degrees cps 0))]
     :note-octave-transpositions [0 1 2]}))
  )
