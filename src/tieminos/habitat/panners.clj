(ns tieminos.habitat.panners
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.panners.trayectory-panner :refer [trayectory-pan-4ch]]
   [tieminos.habitat.routing :refer [input-number->bus]]
   [tieminos.overtone-extensions :as oe :refer [defsynth]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [ctl-synth]]))

(defsynth rand-pan4
  [in 0
   out 0
   amp 1
   rate 0.1
   release 2
   width 2
   orientation 0.5
   gate 1]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/lf-noise1 rate)
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
   rate 0.1
   direction 1
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
             (* amp (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                               gate
                               :action o/FREE)))))

(defonce current-panners
  ;; "A map of input (int) to {synth, type}"
  (atom {}))

(defn stop-panner!
  [in]
  (let [in* (input-number->bus in)
        current-panner (get @current-panners in*)]
    (println current-panner)
    (try (when (:synth current-panner)
           (o/ctl (:synth current-panner) :gate 0))
         (catch Exception e (timbre/error e)))
    (swap! current-panners dissoc in*)))

(comment
  (stop-panner! 5))

(defn panner [{:keys [in type out amp trayectory]
               :or {amp 1} :as args}]
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
        new-panner (case type
                     :clockwise (circle-pan-4ch
                                 {:group (groups/mid)
                                  :in in*
                                  :out out*
                                  :amp amp})
                     :counter-clockwise (circle-pan-4ch
                                         {:group (groups/mid)
                                          :in in*
                                          :direction -1
                                          :out out*
                                          :amp amp})
                     :rand (rand-pan4
                            {:group (groups/mid)
                             :in in*
                             :out out*
                             :amp amp})
                     :trayectory (trayectory-pan-4ch
                                  {:group (groups/mid)
                                   :in in*
                                   :out out*
                                   :trayectory trayectory
                                   :amp amp}))]

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
