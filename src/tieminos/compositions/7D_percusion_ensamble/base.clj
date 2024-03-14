(ns tieminos.compositions.7D-percusion-ensamble.base
  (:require
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.midi.core :refer [get-iac2!]]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori dorian-hexanies-in-polydori-1
            dorian-hexanies-in-polydori-2]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [map-subscale-degs]]
   [time-time.dynacan.players.gen-poly :as gp :refer [ref-rain]]
   [time-time.standard :refer [rrand]]))

(def sink (midi/midi-out "VirMIDI"))
(def iac2 (get-iac2!))

(def root
  "195.7765119449384"
  (* 2/3 (conv/midi->cps 62)))

(defn bh
  "Blackhole outs (starts on chan 3 of Blackhole)"
  [out]
  (+ 22 out))

(defn my-malgo
  [config]
  (malgo-note (merge {:sink sink
                      :scale-size 29
                      :base-midi-deg 60
                      :base-midi-chan 0}
                     config)))

#_{:clj-kondo/ignore [:deprecated-var]}
(def subscales-list-map {:original dorian-hexanies-in-polydori
                      :sorted dorian-hexanies-in-polydori-1
                      :modal dorian-hexanies-in-polydori-2})

(defn diat->polydori-degree
  ([scale degree] (diat->polydori-degree scale degree :original))
  ([scale degree & [type]]
   (let [scales-list* (subscales-list-map type)
         scales-list (or scales-list* (subscales-list-map :original))]
     (when-not scales-list*
       (timbre/error (format "Non-existent `type`: %s. Using `:original` instead." type)))
     (map-subscale-degs (count (:scale polydori-v2))
                        (:degrees
                         (nth scales-list scale))
                        degree))))

(defn deg->freq
  "NOTE: scale is an index to `scales-list`. `type` is the key of `scales-list-map`."
  [& {:keys [base-freq scale degree type]
      :or {base-freq root
           type :original}
      :as config}]
  (scale/deg->freq (:scale polydori-v2)
                   base-freq
                   (diat->polydori-degree scale degree type)))

(def sort-freq->chan-map
  (memoize (fn [freq->chan-map]
             (->> freq->chan-map
                  (map first)
                  (cons -1)
                  sort
                  (partition 2 1)))))

(defn freq->out
  "The key of the `freq->chan-map` is the top freq limit.
  All frequencies at or below this will go to the specified channel.
  For the highest limit, all frequencies above it will also go to the same channel."
  [freq->chan-map freq]
  (let [sorted-list (sort-freq->chan-map freq->chan-map)]
    (reduce
     (fn [chan [lower higher]]
       (if (< lower freq (inc higher))
         (reduced (freq->chan-map higher))
         chan))
     (freq->chan-map (second (last sorted-list)))
     sorted-list)))

(def mfreq->out (memoize freq->out))

(o/defsynth low
  [freq 85
   amp 0.5
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 15) (+ freq 15))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(o/defsynth short-plate
  [freq 200
   amp 0.5
   mod-freq 1000
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 200) (+ freq 200))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(def synths (map #(partial % (groups/early)) [low short-plate]))

(defn init! []
  (groups/init-groups!))

(def mempan
  (memoize (fn [deg-mod]
             (rrand -1.0 1))))

(defn sub-rain
  [{:keys [ref durs on-event ratio]
    :or {ratio 1/9}}]
  (ref-rain (cond-> {:id (keyword "sub-rain" (str (random-uuid)))
                     :loop? false
                     :durs durs
                     :ratio ratio
                     :on-event on-event}
              ref (assoc :ref ref))))
