(ns tieminos.sc-utils.ndef.v1
  "A basic Ndef implementation"
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [tieminos.utils :refer [ctl-synth2]]))

(defonce ndefs (atom {}))

(defmacro ndef
  [id synth & {:keys [out fade-time]
               :or {out 0 fade-time 3}}]
  `(let [synth# ((o/synth
                  [{:name :gate
                    :default (clojure.core/float (overtone.sc.node/to-id 1))
                    :rate :kr}]
                  (o/out ~out
                         (oc/* (o/env-gen
                                (o/asr ~fade-time 1 ~fade-time)
                                :gate ~(symbol "gate")
                                :action o/FREE)
                               ~synth))))]

     (when-let [prev-synth# (get @ndefs ~id)]
       (ctl-synth2 prev-synth# :gate 0))
     (swap! ndefs assoc ~id synth#)))

(defn stop [& ids]
  (doseq [id ids]
    (when-let [prev-synth# (get @ndefs id)]
      (ctl-synth2 prev-synth# :gate 0)
      (swap! ndefs dissoc id))))

(comment
  (do
    (reset! ndefs {})
    (macroexpand-1 '(ndef-synth
                     :my-id
                     (* 0.2 (o/sin-osc 200))
                     {:out 0})))

  (ndef :my-id (* 0.2 (o/sin-osc 100)))
  (stop :my-id)
  (o/stop))
