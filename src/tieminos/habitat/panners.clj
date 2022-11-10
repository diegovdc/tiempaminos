(ns tieminos.habitat.panners
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.overtone-extensions :as oe :refer [defsynth]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [ctl-synth]]))

(defsynth rand-pan4
  [in 0
   out 0
   amp 1
   lfo-freq 0.3
   release 2
   width 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/lf-noise1 lfo-freq)
                           :width width
                           :orientation orientation)
             (* amp (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))

(defsynth circle-pan-4ch
  ;; For circular motion use `direction` 1 or -1 only
  [in 0
   out 0
   amp 1
   lfo-freq 0.3
   direction 1
   release 2
   width 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/lf-saw (* direction lfo-freq))
                           :width width
                           :orientation orientation)
             (* amp (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))

(defonce current-panners
  ;; "A map of input (int) to {synth, type}"
  (atom {}))

(defn panner [{:keys [in type out] :as _args}]
  (let [current-panner (get @current-panners in)
        new-panner (case type
                     :clockwise (circle-pan-4ch
                                 (groups/mid)
                                 :in in
                                 :out out)
                     :counter-clockwise (circle-pan-4ch
                                         (groups/mid)
                                         :in in
                                         :direction -1
                                         :out out)
                     :rand (rand-pan4
                            (groups/mid)
                            :in in
                            :out out))]

    (try (when (:synth current-panner)
           (o/ctl (:synth current-panner) :gate 0))
         (catch Exception e (timbre/error e)))
    (swap! current-panners assoc in {:synth new-panner
                                     :type type
                                     :out out})))

(defn panner-rate [{:keys [in rate] :as _args}]
  (let [panner (get-in @current-panners [in :synth])]
    (ctl-synth panner :rate rate)))
