(ns tieminos.sc-utils.ctl.v1
  (:require
   [clojure.core.async :as a]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.utils :refer [ctl-synth2 iter-async-call2]]))

(comment
  ;; IDEA
  (with-ctls
    synth
    dur                                 ; for the control-opts
    uncontrolled-opts                   ; {}
    {:amp {:type :continuous ; other types :lorentz :discontinuous
           :vals [0 1 0.6 0.2 0]
           ;; optional, else equal divisions of dur
           :durs [1 2 3 2]}})
  ;; =>
  {:synth running-synth
   :controller-id #{:ctl-refrain/amp+some-id}})

(defn- add-increments-to-ctl-interpolation-params
  [total-steps params-start-and-end]
  (->> params-start-and-end
       (map (fn [[k {:keys [start end] :as data}]]
              [k (assoc data :increment (/ (- end start) total-steps))]))
       (into {})))

(comment
  (add-increments-to-ctl-interpolation-params 20 {:a {:start 0 :end 200}
                                                  :b {:start 200 :end 0}}))

(defn ctl-interpolation
  "`params` should be {:param {:start val :end val}}"
  [{:keys [dur-s step-ms synth params delay-s]
    :or {delay-s 0}}]
  (let [steps (/ (* 1000 dur-s) step-ms)
        params-with-increments (add-increments-to-ctl-interpolation-params
                                steps params)]
    (if-not (o/node-active? synth)
      (timbre/error "Could not control synth as it is not active or not a node")
      (a/go
        (a/<! (a/timeout (* 1000 delay-s)))
        (iter-async-call2 step-ms
                          (fn [{:keys [index stop-chan-fn]}]
                            (if (< index steps)
                              (ctl-synth2 synth
                                          (mapcat (fn [[param {:keys [start increment]}]]
                                                    [param (+ start (* increment index))])
                                                  params-with-increments))
                              (stop-chan-fn))))))))

(comment
  (o/defsynth sini [freq 300]
    (o/out 0 (* 0.2 [(o/sin-osc freq) (o/sin-osc freq)])))

  (def s (sini))
  (o/kill s)
  (ctl-interpolation {:delay-s 5
                      :dur-s 5
                      :step-ms 10
                      :synth s
                      :params {:freq {:start 200 :end 400}}})
  (ctl-interpolation {:dur-s 5
                      :step-ms 10
                      :synth s
                      :params {:freq {:start 400 :end 200}}})
  (o/stop)

  (apply o/ctl s (seq {:freq 400})))
