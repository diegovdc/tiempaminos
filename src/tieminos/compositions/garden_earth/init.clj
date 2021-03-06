(ns tieminos.compositions.garden-earth.init
  "Use for initializing `garden-earth` and aiding development"
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base :as ge-base :refer [early-g]]
   [tieminos.compositions.garden-earth.synths.fx :as fx]
   [tieminos.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [tieminos.compositions.garden-earth.synths.live-signal :as live-signal]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.core :refer [connect]]))

(declare startup test-sound load-test-samples! init-groups-and-fx!)

(comment
  ;; antes inicializar SuperCollider con `sc-init.scd`
  (startup true)
  (test-sound)
  (load-test-samples!)
  (init-groups-and-fx!))

(defn load-test-samples! []
  (let [ss (rec/load-own-samples!
            :buffers-atom rec/bufs
            :prefixes-set #{:test}
            :dissoc-prefixes #{})]
    (timbre/info (count @ss) "test samples loaded: " (keys @ss))))

(defn init-groups-and-fx! []
  (let [main (o/group "get-on-the-bus main")
        early (o/group :head main)
        fx (o/group "fx" :after early)]
    (reset! ge-base/groups {:main main :early early :fx fx})
    (reset! ge-base/fx {:rev-l (fx/rev [:tail fx] 8 0 :amp 2)
                        :rev-r (fx/rev [:tail fx] 9 1 :amp 2)
                        :rev-3 (fx/rev [:tail fx] 10 2 :amp 2)
                        :rev-4 (fx/rev [:tail fx] 11 3 :amp 2)})))

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
  (sini (early-g) 8)
  (println :rev-sound)
  (Thread/sleep 1000)
  :done)

(defn startup [dev?]
  (if (o/server-connected?)
    (timbre/info "Server already connected")
    (timbre/info (connect)))
  (init-groups-and-fx!)
  (when dev?
    (load-test-samples!)
    (test-sound)
    (println "Local functions: " (keys (ns-publics *ns*))))
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
