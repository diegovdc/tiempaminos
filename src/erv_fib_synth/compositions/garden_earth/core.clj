(ns erv-fib-synth.compositions.garden-earth.core
  "Use for initializing `garden-earth` and aiding development"
  (:require
   [erv-fib-synth.compositions.garden-earth.base :as ge-base]
   [erv-fib-synth.compositions.garden-earth.synths.fx :as fx]
   [erv-fib-synth.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [erv-fib-synth.compositions.garden-earth.synths.recording :as rec]
   [erv-fib-synth.compositions.garden-earth.synths.live-signal :as live-signal]
   [erv-fib-synth.core :refer [connect disconnect]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]))

(comment
  ;; antes inicializar SuperCollider con `sc-init.scd`
  (startup true))

(defn load-test-samples! []
  (let [ss (rec/load-own-samples!
            :buffers-atom rec/bufs :prefixes-set #{:test}
            :dissoc-prefixes #{})]
    (timbre/info (count @ss) "test samples loaded: " (keys @ss))))

(defn init-groups-and-fx! []
  (let [main (o/group "get-on-the-bus main")
        early (o/group :head main)
        fx (o/group "fx" :after early)]
    (reset! ge-base/groups {:main main :early early :fx fx})
    (reset! ge-base/fx {:rev-l (fx/rev [:tail fx] 8 :amp 2)
                        :rev-r (fx/rev [:tail fx] 9 1 :amp 2)})))
(defn init-fx! []
  (let [fx (@ge-base/groups :fx)]
    (reset! ge-base/fx {:rev-l (fx/rev [:tail fx] 8 :amp 2)
                        :rev-r (fx/rev [:tail fx] 9 1 :amp 2)})))

(o/defsynth sini [out 0]
  (o/out out (* (o/pan2 (o/sin-osc 200) 0) 0.2 (o/env-gen (o/env-perc) :action o/FREE))))

(defn test-sound []
  (sini)
  (println :ordinary-sound)
  (Thread/sleep 1000)
  (sini [:head 30] 8)
  (println :rev-sound)
  (Thread/sleep 1000)
  :done)

(defn startup [dev?]
  (timbre/info (connect 2))
  (init-groups-and-fx!)
  (when dev?
    (load-test-samples!)
    (test-sound)
    (keys (ns-publics *ns*)))
  :done)

(def stop ge-base/stop)

(defn a+53-tuner []
  (mapv #(tuning-monitor
          (-> ge-base/eik-notes (get "A+53") :bounded-ratio (* %))
          1 50 0.4)
        [220 440 880]))
(comment (a+53-tuner))


(defn start-pitch-tracking []
  (live-signal/run-receive-pitch)
  (live-signal/run-get-signal-pitches))

(comment (def tracker-synth (start-pitch-tracking)))

(keys (ns-publics *ns*))
