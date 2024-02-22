(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.refrains
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.cps.utils :as cps-utils]
   [erv.scale.core :as scale]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.synth-tunings.cps-19-31-43-55-79-91 :as cps-19-31-43-55-79-91]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :refer [norm-amp silence?]]
   [tieminos.habitat.routing :refer [main-returns]]
   [tieminos.habitat.scratch.sample-rec2 :refer [periodize-durs
                                                 rand-latest-buf]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.utils :refer [rrange]]
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
                                                        {:period (rand-nth periods)})))))))))

(comment
  (gp/stop)
  (chords1
   (let [cps (-> cps-19-31-43-55-79-91/hexanies first second cps-utils/+degrees)]
     {:cps cps
      :chords [(first (cps-utils/harmonic-triad-degrees cps 0))]
      :note-octave-transpositions [0 1 2]})))

(defn hacia-un-nuevo-universo-perc-refrain-v2p2
  "This version can handle rate chords (as a vector of rates)"
  [{:keys [refrain-id web-visualizer-fn-id
           buf-fn period durs rates amp
           amp-fn ;; optional, takes the index and returns an amp value, if present `amp` will be overriden
           d-weights d-level-weights a-weights room-weights
           out-bus
           silence-thresh
           on-play
           refrain-ratio]
    :or {refrain-id ::hacia-un-nuevo-universo-perc3
         web-visualizer-fn-id ::hacia-un-nuevo-universo-1
         buf-fn rand-latest-buf
         period 2.5
         durs (bzs/fsf 20 0.1 1)
         rates (range 1 10)
         amp 1
         a-weights {(rrange 0.01 0.1) 10
                    (rrange 2 5) 1/2}
         d-weights {(rrange 0.2 0.3) 5
                    (rrange 0.3 0.5) 3
                    (rrange 0.5 1) 1
                    (rrange 1 5) 1/2}
         d-level-weights {0.3 1}
         room-weights {0.2 2, 2 1/2 4 1/2}
         out-bus (main-returns :non-recordable)
         silence-thresh 0.05
         refrain-ratio 1}}]
  (let [rates* (map (fn [r] (if (sequential? r) r [r])) rates)]
    (ref-rain
     :id refrain-id
     :durs (periodize-durs period durs)
     :ratio refrain-ratio
     :on-event (on-event
                (when-let [buf (buf-fn {:index index})]
                  (when-not (silence? silence-thresh buf) ;; allow us to control silences by not playing
                    (let [rate (at-i rates*)
                          amp* (if amp-fn (amp-fn index) amp)]
                      (doseq [r rate]
                        (let [start 0 #_(rrange (rrange 0 0.5) 0.7)
                              end 1 #_(+ start (rrange 0.05 0.3))
                              a (weighted a-weights)
                              trig-rate (+ 90 (rand-int 20))
                              config {:group (groups/mid)
                                      :buf buf
                                      :a a
                                      :d (/ (+ (/ a 2) (weighted d-weights))
                                            2)
                                      :r (+ (/ a 2) (weighted d-weights))
                                      :d-level (weighted d-level-weights)
                                      :rev-room (weighted room-weights)
                                      :trig-rate 100
                                      :grain-dur (/ 1 (/ trig-rate 2))
                                      :amp-lfo (rrange 0.1 0.4)
                                      :amp-lfo-min 0.95
                                      :lpf-max (rrange 2000 10000)
                                      :start start
                                      :end end
                                      :out out-bus
                                      :pan (rrange -1 1)}]

                          (doseq [synth-args [(assoc config
                                                     :rate (float r)
                                                     :interp (rand-nth [1 2 4])
                                                     :amp (* amp* (rrange 0.2 1) (norm-amp buf)))
                                              (assoc config
                                                     :rate (* (rand-nth [2 3/2 5/4 7/4 1/2 1 1 1 1]) r)
                                                     :interp (rand-nth [4])
                                                     :amp (* amp* (rrange 0 0.7) (norm-amp buf)))]]
                            (println "=============BUFF Duration" (:duration buf))
                            (println "=============SYNTH Duration"
                                     (apply + (vals (select-keys config [:a :d :r])))
                                     (select-keys config [:a :d :r]))
                            (on-play {:refrain/data data
                                      :meta {:context/web-visualizer-fn-id web-visualizer-fn-id} ;; TODO
                                      :synth-args synth-args})))))))))))
