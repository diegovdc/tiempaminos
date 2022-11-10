(ns tieminos.habitat.quad-inputs
  (:require
   [clojure.edn :as edn]
   [clojure.math :refer [round]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.habitat.quad-inputs :refer [args->map]]
   [tieminos.habitat.resonance-panner :as reso-pan]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]))

(o/defsynth rev [in 0 out 0 mix 0.5 room 0.8 damp 0.5 amp 1]
  (o/out out (* amp (o/free-verb (o/in in) mix room damp))))

(o/defsynth rand-pan4
  [in 0
   out 0
   lfo-freq-x 0.3
   lfo-freq-y 0.3
   release 2
   gate 1]
  (o/out out
         (-> (o/pan4:ar
              (o/in in)
              (o/lf-noise1 lfo-freq-x)
              (o/lf-noise1 lfo-freq-y))
             (* (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                           gate
                           :action o/FREE)))))

(o/defsynth circle-pan-4ch
  ;; For circular motion use `direction` 1 or -1 only
  [in 0
   out 0
   rate 0.2
   release 2
   direction 1
   gate 1]
  (o/out out
         (->
          (o/pan4
           (o/in in)
           (o/sin-osc rate 0)
           (o/sin-osc rate (* direction (/ Math/PI 2))))
          (* (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                        gate
                        :action o/FREE)))))
(o/defsynth circle-pan
  ;; For circular motion use `direction` 1 or -1 only
  [in 0
   out 0
   rate 0.2
   release 2
   direction 1
   gate 1]
  (o/out out
         (->
          (o/pan-az
           4
           (o/in in)
           (o/lf-saw rate)
           :width 0.9)
          (* (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                        gate
                        :action o/FREE)))))

(oe/defsynth sini
  [freq 200 amp 0.5 out 0]
  (o/out out (* amp (o/sin-osc freq))))

(oe/defsynth sini-2
  [amp 0.5 out 0]
  (o/out out (* (o/sin-osc 200)
                (o/env-gen (o/env-perc) :action o/FREE)
                amp)))

(oe/defsynth whitey
  [amp 0.2 out 0]
  (o/out out (* amp (o/white-noise))))

(defn args->map [args]
  (try
    (->> args (partition 2 2)
         (map (fn [[k v]]
                {(keyword k) (cond (and (= "in" k) (string? v)) (edn/read-string v)
                                   (string? v) (keyword v)
                                   :else v)}))
         (apply merge))
    (catch Exception _
      (throw (ex-info "Could not convert args to map"
                      {:args args})))))

(def input->panner-input
  {0 14
   1 15
   2 16})

(defonce current-panners
  ;; "A map of input (int) to {synth, type}"
  (atom {}))

(defn ctl-synth [synth & params]
  (when synth
    (try (apply o/ctl synth params)
         (catch Exception e (timbre/error e)))))

(def main-pre-out
  "Out that sends to the reverbs"
  8)

(defn panner [{:keys [in type] :as _args}]
  (let [current-panner (get @current-panners in)
        in* (input->panner-input in)
        new-panner (case type
                     :clockwise (circle-pan-4ch
                                 (groups/mid)
                                 :in in*
                                 :out main-pre-out)
                     :counter-clockwise (circle-pan
                                         (groups/mid)
                                         :in in*
                                         :direction -1
                                         :out main-pre-out)
                     :rand (rand-pan4
                            (groups/mid)
                            :in in*
                            :out main-pre-out))]

    (try (when (:synth current-panner)
           (o/ctl (:synth current-panner) :gate 0))
         (catch Exception e (timbre/error e)))
    (swap! current-panners assoc in {:synth new-panner
                                     :type type})))

(defn panner-rate [{:keys [in rate] :as _args}]
  (let [panner (get-in @current-panners [in :synth])]
    (ctl-synth panner :rate rate)))

(defn map-value
  "Maps a `value-key` from an `args-map` between `min*` and `max*`,
  assuming the input `value-key` refers to a number between `0` and `1`
  The `value-key` defaults to `:value`"
  ([args-map min* max*] (map-value :value args-map min* max*))
  ([value-key args-map min* max*]
   (update args-map
           value-key #(->> %
                           (* max*)
                           round
                           (max min*)
                           ;; ensure it doesn't go beyond stated max, if value is > 1
                           (min max*)))))

(defonce fx (atom {}))

(defn init-fx! []
  (let [fx-g (groups/fx)]
    (reset! fx
            {:rev-1 (rev fx-g 8 0  :amp 2)
             :rev-2 (rev fx-g 9 1 :amp 2)
             :rev-3 (rev fx-g 10 2 :amp 2)
             :rev-4 (rev fx-g 11 3 :amp 2)})))

(comment
  (doseq [[_ rev] @fx]
    (o/ctl rev :damp 0)))

(oe/defsynth instrument-in
  [in 0
   amp 1
   out 0]
  (o/out out (* amp (o/sound-in in))))

(comment
  #_(osc/osc-debug false)
  (habitat-osc/init)
  (habitat-osc/responder
   (fn [{:keys [path args] :as msg}]
     (println path args)
     (let [args-map (args->map args)]
       (case path
         "/panner" (panner args-map)
         "/panner-rate" (panner-rate args-map)
         "/reso-pan-voices" (reso-pan/update-state :voices (map-value args-map 1 10))
         "/reso-pan-dur" (reso-pan/update-state :dur (map-value args-map 5 60))
          ;; TODO add reso-pan-amp
         "/reso-pan" (reso-pan/trigger (:in args-map) main-pre-out 5 10)
         (println "Unknown path for message: " msg)))))

  (o/stop)
  (do :init-sequence
      (reset! current-panners {})
      (groups/init-groups!)
      (init-fx!))

  (tieminos.core/rec "raspados" :n-chans 4)
  (o/recording-stop)

  (def guitarra (let [bus 14]
                  (rand-pan4 (groups/mid) bus main-pre-out)
                  (instrument-in {:group (groups/early)
                                  :in 0
                                  :out bus})))
  (o/ctl guitarra :amp 2.5)
  (def micro-1 (let [bus 15]
                 (rand-pan4 (groups/mid) bus main-pre-out)
                 (instrument-in {:group (groups/early)
                                 :in 1
                                 :out bus})))
  (o/ctl micro-1 :amp 1)
  (def micro-2 (let [bus 16]
                 (rand-pan4 (groups/mid) bus main-pre-out)
                 (instrument-in {:group (groups/early)
                                 :in 2
                                 :out bus})))
  (o/ctl micro-2 :amp 1)
  (def micro-3 (let [bus 16]
                 (rand-pan4 (groups/mid) bus main-pre-out)
                 (instrument-in {:group (groups/early)
                                 :in 3
                                 :out bus})))
  (def micro-4 (let [bus 16]
                 (rand-pan4 (groups/mid) bus main-pre-out)
                 (instrument-in {:group (groups/early)
                                 :in 4
                                 :out bus}))))

(comment
    ;; test
  (o/stop)
  (sini {:group (groups/early) :out (input->panner-input 0)})
  (sini {:group (groups/early) :freq 500 :out (input->panner-input 1)})
  (panner {:in 1 :type :clockwise})
  (panner-rate {:in 0 :rate 2})
  (o/ctl (:synth (get @current-panners 1)) :gate 0))

(comment
  ;; remapping pan-az channels
  (defn circle-az
    [pan-az-out]
    (let [[a b c d]
          pan-az-out]
      [a b d c]))
  (o/demo 20 (circle-az (o/pan-az
                         4
                         (o/saw 400)
                         :pos (o/lf-saw:kr -0.2)
                         :width 1.75))))

