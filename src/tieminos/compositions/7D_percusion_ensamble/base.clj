(ns tieminos.compositions.7D-percusion-ensamble.base
  (:require
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.core]
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

(comment
  (midi/midi-devices)
  (midi/midi-out "VirMIDI")
  (midi/midi-out "Bus 3"))
(def surge-suave (midi/midi-out "VirMIDI"))
(def ssuave surge-suave)
(def plasmonic-bell (midi/midi-out "Bus 3"))
(def pbell plasmonic-bell)
(def iac2 (get-iac2!))

(def root
  "195.7765119449384"
  (* 2/3 (conv/midi->cps 62)))

(defn bh
  "Blackhole outs (starts on chan 3 of Blackhole)"
  [out]
  (+ 22 out))

(defn my-malgo
  "Takes and `malgo-note` config with an extra `:sinks` key which an be an array of sinks or a single sink.
  The if multiple sinks are used the same event will be dispatched to each sink

  NOTE: The default `sink` is `surge-suave`."
  [{:keys [sinks] :as config}]
  (let [defaults {:sink surge-suave
                  :scale-size 29
                  :base-midi-deg 60
                  :base-midi-chan 0}]

    (if-not (seq sinks)
      (malgo-note (merge defaults config))
      (doseq [sink sinks]
        (malgo-note (-> defaults
                        (merge config)
                        (dissoc :sinks)
                        (assoc :sink sink)))))))

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
       (timbre/error (ex-info (format "Non-existent `type`: %s. Using `:original` instead.
If using `mdeg->freq` this may show up only once because it is memoized, even if the unintended value is being used more than once." type)
                              {:scale scale :degree degree :type type})))
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

(def mdeg->freq (memoize deg->freq))

(def sort-freq->chan-map
  (memoize (fn [freq->chan-map]
             (->> freq->chan-map
                  (map first)
                  (cons -1)
                  sort
                  (partition 2 1)))))

(def default-out-map {130 (bh 0)
                      523 (bh 2)
                      1046 (bh 4)
                      2092 (bh 6)})

(defn ^:deprecated freq->out
  ;; NOTE this doesn't really work
  "The key of the `freq->chan-map` is the top freq limit.
  All frequencies at or below this will go to the specified channel.
  For the highest limit, all frequencies above it will also go to the same channel."
  ([freq] (freq->out default-out-map freq))
  ([freq->chan-map freq]
   (let [sorted-list (sort-freq->chan-map freq->chan-map)]
     (reduce
      (fn [chan [lower higher]]
        (if (< lower freq (inc higher))
          (reduced (freq->chan-map higher))
          chan))
      (freq->chan-map (second (last sorted-list)))
      sorted-list))))

(def ^:deprecated  out (memoize freq->out))

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
  (when (o/server-disconnected?)
    (tieminos.core/connect))
  (groups/init-groups!))

(defn stop!
  "Stop all refrains except those from `:tieminos-algo-note` because they stop themselves."
  []
  (doseq [id (->> @gp/refrains
                  keys
                  (remove (fn [k] (and (keyword? k)
                                       (= "tieminos-algo-note"  (namespace k))))))]
    (gp/stop id)))

(def mempan
  (memoize (fn [_deg-mod]
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

(defn vel
  "Ensure valid midi velocity"
  [vel]
  (-> vel
      int
      (min 127)
      (max 0)))
