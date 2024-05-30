(ns tieminos.habitat.panners
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.panners.trayectory-panner :refer [trayectory-pan-4ch]]
   [tieminos.habitat.routing :refer [input-number->bus]]
   [tieminos.overtone-extensions :as oe :refer [defsynth]]
   [tieminos.habitat.groups :as groups]
   [tieminos.utils :refer [ctl-synth]]))

(defsynth rand-pan4
  [in 0
   out 0
   amp 1
   rate 0.1
   release 2
   width 2
   a 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/lf-noise1 rate)
                           :width width
                           :orientation orientation)
             (* amp (o/env-gen (o/env-adsr a 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))

(defsynth rand-pan8
  [in 0
   out 0
   amp 1
   rate 0.1
   release 2
   width 2
   a 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 8
                           :in (o/in in)
                           :pos (o/lf-noise1 rate)
                           :width width
                           :orientation orientation)
             (* amp (o/env-gen (o/env-adsr a 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))
(defsynth rand-pan4v2
  [in 0
   out 0
   amp 1
   rate 0.1
   release 2
   width 2
   a 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/lf-noise2 rate)
                           :width width
                           :orientation orientation)
             (* amp (o/env-gen (o/env-adsr a 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))

(defsynth circle-pan-4ch
  ;; For circular motion use `direction` 1 or -1 only
  [in 0
   out 0
   amp 1
   rate 0.1
   direction 1
   a 2
   release 2
   width 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/lf-saw (* direction rate))
                           :width width
                           :orientation orientation)
             (* amp (o/env-gen (o/env-adsr a 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))

(defonce current-panners
  ;; "A map of input (int) to {synth, type}"
  (atom {}))

(defn get-current-panner [in]
  (let [in* (input-number->bus in)]
    (get @current-panners in*)))
(-> @current-panners)
(defn stop-panner!
  [in]
  (let [in* (input-number->bus in)
        current-panner (get @current-panners in*)]
    (try (if (:synth current-panner)
           (o/ctl (:synth current-panner) :gate 0)
           (timbre/warn "No panner to stop with:" in))
         (catch Exception e (timbre/error e)))
    (swap! current-panners dissoc in*)))

(comment
  (stop-panner! 5)
  (stop-panner! 5)
  (->> @current-panners
       keys))

(defn panner
  "Creates a panner. If a panner for the `input` already exists then it creates a new instance and closes the gate of the previous panner.
  So there should always be one active panner for every input (except of course for the fadeout of the old panner)"
  [{:keys [in type out]
    :as args}]
  (when-not (or in out)
    (throw (ex-info "Can not modify panner, data missing"
                    args)))
  (let [in* (input-number->bus in)
        current-panner (get @current-panners in*)
        out* (or out (:out current-panner))
        _ (when-not out* (throw (ex-info "Current panner has no `out`, please set it in the `current-panners` atom"
                                         {:input-args args
                                          :current-panner current-panner
                                          ::current-panners @current-panners})))
        args* (dissoc args :in :type :out :group)
        new-panner (case type
                     :clockwise (circle-pan-4ch
                                 (merge {:group (groups/panners)
                                         :in in*
                                         :out out*}
                                        args*))
                     :counter-clockwise (circle-pan-4ch
                                         (merge {:group (groups/panners)
                                                 :in in*
                                                 :direction -1
                                                 :out out*}
                                                args*))
                     :rand (rand-pan4
                            (merge {:group (groups/panners)
                                    :in in*
                                    :out out*}
                                   args*))
                     :rand-2 (rand-pan4v2
                              (merge {:group (groups/panners)
                                      :in in*
                                      :out out*}
                                     args*))
                     :trayectory (trayectory-pan-4ch
                                  (merge {:group (groups/panners)
                                          :in in*
                                          :out out*}
                                         args*))
                     ;; octophonic
                     :rand8 (rand-pan8
                             (merge {:group (groups/panners)
                                     :in in*
                                     :out out*}
                                    args*)))]

    (try (when (:synth current-panner)
           (o/ctl (:synth current-panner) :gate 0))
         (catch Exception e (timbre/error e)))
    ;; TODO maybe refactor the key `in` into `id` or something...
    (swap! current-panners assoc in* {:in in*
                                      :synth new-panner
                                      :type type
                                      :out out*})))

(defn panner-rate [{:keys [in rate max]
                    :or {max 1.5} :as _args}]
  (let [panner (get-in @current-panners [(input-number->bus in) :synth])]
    (ctl-synth panner :rate (* max rate))))
