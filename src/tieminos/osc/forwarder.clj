(ns tieminos.osc.forwarder
  (:require
   [tieminos.osc.core :as osc]
   [tieminos.osc.mpe :as mpe]))

(defonce config (atom {}))

(defn init [])

(comment
  (require '[erv.cps.core :as cps]
           '[clojure.set :as set]
           '[erv.utils.core :as utils]
           '[erv.utils.core :as utils]
           '[erv.utils.conversions :as conv]
           '[overtone.core :as o]
           '[erv-fib-synth.midi.mpe :as midi-mpe]
           '[erv-fib-synth.synths :as synths]
           '[erv.scale.core :as scale]
           '[erv.neji.core :as neji]
           '[erv.edo.core :as edo])

  (o/connect-external-server)
  (o/defsynth sini
    [freq 200
     amp 1
     pan 0
     atk 0.2
     dcy 0.7
     sust 0.3
     rel 0.5
     gate 1
     out 0]
    (o/out out
           (-> (o/sin-osc freq)
               (* 0.1 (o/env-gen (o/env-adsr atk dcy sust rel)
                                 :gate gate :action o/FREE))
               (o/pan2 0))))
  (sini)
  (sini)
  (o/clear)

  (o/recording-start "/home/diego/Desktop/happy-song-a-la-chiapaneca")
  (o/recording-stop)
  (-> @osc/synths)
  (cps/make 2 [1 3 5 7])

  (add-watch osc/synths nil
             (fn [_ _ _ state]
               (->> (keys state)
                    (map #(-> (utils/wrap-at (-  % 60)  scale)
                              :set))
                    (#(if (seq %)
                        (let [diff (apply set/intersection %)]
                          [diff
                           (set/difference (apply set/union %) diff)
                           (set %)]) %))
                    println)))
  (def scale (:scale (cps/make 2 [1 3 5 7])))
  (def scale (:scale (neji/make 12 23)))
  (def scale (:scale (edo/from-pattern (repeat 31 1))))
  (midi-mpe/deg->mpe-note scale 200 (- 54 60))
  (-> scale #_(nth 1))
  (-> scale (nth 3) :bounded-ratio (* 200) sini)
  (osc/midi-event :note-on println)
  (defn get-note [midi-note]
    (scale/deg->freq scale 200 (- midi-note 60)))
  (osc/all-notes-off)
  (osc/midi-event
   :note-on #(do
               #_(sini (scale/deg->freq scale 200 (- (:note %) 60)
                                        :debug-fn (juxt :bounded-ratio :diff))
                       (/ (:vel %) 150)
                       :sust 0.5
                       :rel 0.1)

               #_(synths/low2 (get-note (:note %))
                              (+ 0.2 (/ (:vel %) 100))
                              :mod-amp (+ 0.1 (/ (:vel %) 140))
                              :mod-freq 4000)
               (mpe/mpe-note-on
                :scale scale
                :base-freq 200
                :deg-offset -60
                :get-pitch-class mpe/get-cps-pitch-class
                :midi-note (:note %)
                :vel (:vel %)))
   :note-off #(mpe/mpe-note-off (:note %))))
