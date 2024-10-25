(ns tieminos.compositions.garden-earth.moments.two.rains
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.synths
    :refer [simple-playbuf]]
   [tieminos.compositions.garden-earth.moments.two.utils :refer [normalize-rates]]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn rain-simple-playbuf
  [{:keys [id
           rates-fn
           durs-fn
           amp-fn
           rec-query
           delay-fn
           synth
           synth-config
           recent-weight
           old-weight
           out]
    :or {synth simple-playbuf
         amp-fn (fn [_i] 4)
         durs-fn (fn [_] (+ 0.1 (rand 5)))
         delay-fn (fn [{:keys [_index _buf _rate]}] 0)
         recent-weight 2
         old-weight 1
         out (ge.route/out :ndef-1)}}]
  (ref-rain
    :id id
    :durs durs-fn
    :on-event (on-event
                (when-not rec-query
                  (timbre/error "No rec-query"))
                (when-let [buf (habitat.rec/weigthed-rand-queried-buf
                                 {:rec-query rec-query
                                  :recent-amount 2
                                  :recent-weight recent-weight
                                  :old-weight old-weight})]
                  (doseq [rate (normalize-rates (rates-fn index))]
                    (synth
                      (merge {:group (groups/early)
                              :buf buf
                              :rate rate
                              :amp (amp-fn index)
                              :amp-ctl (ge.route/ctl-bus :exp/pedal-1)
                              :amp-ctl-min 0.25
                              :amp-ctl-max (o/db->amp 4)
                              :delay (delay-fn {:buf buf :index index :rate rate})
                              :pan (rrange -1.0 1)
                              :out out}
                             (if (fn? synth-config)
                               (synth-config {:buf buf :index index :rate rate})
                               synth-config))))))))
