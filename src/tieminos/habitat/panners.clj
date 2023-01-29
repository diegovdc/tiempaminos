(ns tieminos.habitat.panners
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.routing :refer [input-number->bus]]
   [tieminos.math.utils :refer [logscale]]
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

(defn trayectory-pan
  "`trayectories` is [{:pos 0 :dur 5 :width 1.2}]. The `:dur` is the transition duration between the current node and the next,
  and the one on the last map is not necessary.
  After peforming the trayectory the panner will stay in the last position until it is released."
  [{:keys [trayectories in out amp release gate]
    :or {trayectories [{:pos 0 :dur 1 :width 2}]
         in 0
         out 0
         amp 1
         release 2}}]
  (println trayectories)
  (let [pos-levels (map :pos trayectories)
        pos-durations (map :dur trayectories)
        widths (map :width trayectories)
        curves (repeat (count trayectories) 0.2)
        release-node (dec (count trayectories))]
    ((o/synth trayectory-pan-synth
              [{:name :gate
                :default (clojure.core/float (overtone.sc.node/to-id 1))
                :rate :kr}]
              (o/out out
                     (-> (oe/circle-az :num-channels 4
                                       :in (o/in in)
                                       :pos (o/env-gen (o/envelope pos-levels pos-durations curves release-node))
                                       :width (o/env-gen (o/envelope widths pos-durations curves release-node))
                                       :orientation 0.5)
                         (* amp (o/env-gen (o/env-adsr 1 1 1 release :curve -0.5)
                                           gate
                                           :action o/FREE)))))
     (groups/mid))))

(comment
  (o/demo)
  (groups/init-groups!)

  (def test-bus (o/audio-bus 1 "test-bus"))
  (def s ((o/synth (o/out test-bus
                          (* 0.2 (o/lpf (o/saw 300) 1500))))
          (groups/early)))

  (o/kill s)
  (def t (trayectory-pan {:out 0 :in test-bus}))

  (def t (trayectory-pan {:trayectories [{:pos -1 :dur 1 :width 1}
                                         {:pos 0.2 :dur 1 :width 4}
                                         {:pos 0.71 :dur 1 :width 1}
                                         {:pos 1.71 :dur 2 :width 2}
                                         {:pos -0.2 :dur 1 :width 1}]
                          :out 0 :in test-bus}))
  (def t (trayectory-pan {:trayectories [{:pos -1 :dur 1 :width 1}
                                         {:pos -1 :dur 1 :width 2}
                                         {:pos -1 :dur 1 :width 3}
                                         {:pos -1 :dur 1 :width 1}
                                         {:pos -1 :dur 1 :width 4}
                                         {:pos -1 :dur 1 :width 1}
                                         {:pos -1 :dur 1 :width 4}
                                         {:pos -1 :dur 1 :width 4}
                                         {:pos -1 :dur 1 :width 1}
                                         {:pos -1 :dur 1 :width 1}
                                         {:pos -1 :dur 1 :width 1}
                                         {:pos -1 :dur 1 :width 1}]
                          :out 0
                          :in test-bus}))
  (o/ctl t :gate 0)
  (o/stop))

(defonce current-panners
  ;; "A map of input (int) to {synth, type}"
  (atom {}))

(defn panner [{:keys [in type out] :as args}]
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
                                  :out out*})
                     :counter-clockwise (circle-pan-4ch
                                         {:group (groups/mid)
                                          :in in*
                                          :direction -1
                                          :out out*})
                     :rand (rand-pan4
                            {:group (groups/mid)
                             :in in*
                             :out out*}))]

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
