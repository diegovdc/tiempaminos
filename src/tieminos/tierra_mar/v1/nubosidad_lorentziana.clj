(ns tieminos.tierra-mar.v1.nubosidad-lorentziana
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as sc.groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.tierra-mar.v1.arp :as tm.arp]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [tieminos.tierra-mar.v1.state :as tm.state]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(comment
  (init!)
  (stop!))

(declare start-lorentzian-osc! nuboso nuboso2 start-synths! stop-synths! stop-lorentzian-osc!)

(defn init! []
  (start-lorentzian-osc!)
  (start-synths!))

(defn stop!
  []
  (stop-synths!)
  (stop-lorentzian-osc!))

(defn send-lorentzian-flow-osc
  [iem-osc-client lorentz-system i]
  (let [i* (* i)
        az (lorentz/bound (lorentz-system i*) :x -180 180)
        el (lorentz/bound (lorentz-system i*) :y -180 180)
        roll (lorentz/bound (lorentz-system i*) :z -180 180)]
    (osc/osc-send iem-osc-client "/StereoEncoder/azimuth" (float az))
    (osc/osc-send iem-osc-client "/StereoEncoder/elevation" (float el))
    (osc/osc-send iem-osc-client "/StereoEncoder/width" (float (/ (+ az el roll)
                                                                  2)))
    (osc/osc-send iem-osc-client "/StereoEncoder/roll"  (float roll))))

(defn start-lorentzian-osc!
  "OSC for the stereo encoders"
  []
  (let [lor (lorentz/init-system :x 0.31 :y 0.01 :z 0.02 :dt 0.05)
        lor2 (lorentz/init-system :x 0.41 :y 0.1 :z 0.082 :dt 0.05)]
    (rain.v2/ref-rain
     :id ::lorentzian-stereo-encoders
     :durs [0.2]
     :on-event
     (rain.v2/on-event
      (send-lorentzian-flow-osc (tm.configs/get-iem-osc-client :nubosidad-lorenztiana-fl)
                                lor (+ 500 i))
      (send-lorentzian-flow-osc (tm.configs/get-iem-osc-client :nubosidad-lorenztiana-fl2)
                                lor2 (+ 1000 i))
      (send-lorentzian-flow-osc (tm.configs/get-iem-osc-client :nubosidad-lorenztiana-arp)
                                lor (+ 1500 i))
      (send-lorentzian-flow-osc (tm.configs/get-iem-osc-client :nubosidad-lorenztiana-arp2)
                                lor2 (+ 2000 i))))))

(defn stop-lorentzian-osc!
  "OSC for the stereo encoders"
  []
  (rain.v2/stop ::lorentzian-stereo-encoders))

(comment
  (rain.v2/stop ::lorentzian-stereo-encoders)
  (o/stop))

;;;;;;;;;;;;;;;;;;
;; Synths
;;;;;;;;;;;;;;;;;;

(oe/defsynth nuboso
  [in 0
   out 0
   amp 1
   rev-mix 1
   rev-room 1
   gate 1]
  (let [sig (->  (o/in in 1)
                 #_(o/mix)
                 (o/moog-ladder 700 0.2)
                 (* 8))
        rev (-> sig
                (o/free-verb rev-mix rev-room 0.7)
                (o/pan2 (lfo-kr 1.1 -0.5 1)))]

    (o/out out (* amp
                  [(+ (* #_(o/db->amp 3) sig)
                      (* #_(o/db->amp -3) (first rev)))
                   (second rev)]
                  (o/env-gen (o/adsr 2 1 1 4)
                             :gate gate
                             :action o/FREE)))))
(oe/defsynth nuboso2
  [in 0
   out 0
   amp 1
   rev-room1 1.3
   rev-room2 1.1
   rev-mix 1
   bpf2-amp 1
   gate 1]
  (let [sig (->  (o/in in 1)
                 #_(o/mix)

                 (o/b-moog (lfo-kr 1.7 300 500)
                           (lfo-kr 1.2 0.3 0.5)
                           1)
                 #_(o/lpf 600)
                 (* 0.4))
        rev (-> sig (o/free-verb rev-mix rev-room1 0.4)
                (* (lfo-kr 1.3 0.4 1.5)))
        rev2 (-> rev (o/pitch-shift 0.2 1/2)
                 (o/free-verb rev-mix rev-room2 0.8)
                 (* (lfo-kr 1.4 0.4 1.5)))]

    (o/out out (* amp
                  (o/mix [[rev rev2]
                          (-> [rev rev2]
                              (o/b-moog (lfo-kr 1.7 300 500)
                                        (lfo-kr 1.2 0.3 0.5)
                                        1)
                              (* bpf2-amp))])
                  (o/env-gen (o/adsr 2 1 1 4)
                             :gate gate
                             :action o/FREE)))))

(comment
  ;; Testing: start and stop synths
  (do
    (defonce testsynths (atom {}))
    (defn add-test-synth!
      [k syn]
      (swap! testsynths assoc k syn))

    (defn kill-test-synths!
      []
      (doseq [[_k syn] @testsynths]
        (try (o/ctl syn :gate 0)
             (catch Exception _e nil)))
      (reset! testsynths {}))

    (kill-test-synths!)

    (add-test-synth!
     :fl
     (nuboso
      {:group (sc.groups/mid)
       :in (ge.route/fl-i1 :bus)
       :out (tm.configs/get-output :nubosidades-fl-2ch)}))
    (add-test-synth!
     :arp
     (nuboso
      {:group (sc.groups/mid)
       :in (tm.configs/get-audio-bus :arp->nubosidad)
       :amp 0.2
       :rev-mix 1
       :rev-room 0.5
       :out (tm.configs/get-output :nubosidades-arp-2ch)}))
    (add-test-synth!
     :fl2
     (nuboso2
      {:group (sc.groups/mid)
       :in (ge.route/fl-i1 :bus)
       :out (tm.configs/get-output :nubosidades-fl2-2ch)}))
    (add-test-synth!
     :arp2
     (nuboso2
      {:group (sc.groups/mid)
       :in (tm.configs/get-audio-bus :arp->nubosidad2)
       :amp 0.1
       :bpf2-amp 0.7
       :rev-room 0.5
       :out (tm.configs/get-output :nubosidades-arp2-2ch)}))))

(defn start-synths!
  []
  (tm.state/add-synth!
   ::fl1
   (nuboso
    {:group (sc.groups/mid)
     :in (ge.route/fl-i1 :bus)
     :out (tm.configs/get-output :nubosidades-fl-2ch)}))
  (tm.state/add-synth!
   ::fl2
   (nuboso2
    {:group (sc.groups/mid)
     :in (ge.route/fl-i1 :bus)
     :out (tm.configs/get-output :nubosidades-fl2-2ch)}))
  (tm.state/add-synth!
   ::arp1
   (nuboso
    {:group (sc.groups/mid)
     :in (tm.configs/get-audio-bus :arp->nubosidad)
     :amp 0.2
     :rev-mix 1
     :rev-room 0.5
     :out (tm.configs/get-output :nubosidades-arp-2ch)}))
  (tm.state/add-synth!
   ::arp2
   (nuboso2
    {:group (sc.groups/mid)
     :in (tm.configs/get-audio-bus :arp->nubosidad2)
     :amp 0.1
     :bpf2-amp 0.7
     :rev-room 0.5
     :out (tm.configs/get-output :nubosidades-arp2-2ch)})))

(defn stop-synths!
  []
  (doseq [k [::fl1 ::fl2 ::arp ::arp2]]
    (try (let [syn (tm.state/get-synth k)]
           (o/ctl syn :gate 0))
         (catch Exception _e nil))))

;; flujo de señal
;; fl -> nuboso 1 y 2
;; fl -> arp -> nuboso 1 y 2
;;
(comment
  (sc.groups/init-groups!))

(comment
  (oe/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc 200)))))

  (oe/defsynth ini
    [in 6
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sound-in in)))))

  (def test-sini (ini {:group (sc.groups/early)
                       :in 6
                       :out (tm.configs/get-audio-bus :arp->nubosidad2)}))

  (o/kill test-sini))

;;;;;;;;;;;;;;;;;;
;; Arp
;;;;;;;;;;;;;;;;;;

(defn start-arp!
  []
  (tm.state/set-arp-pattern! tm.state/state (tm.arp/get-pattern "[0 2]" #_":default"))
  (tm.arp/start-sample-arp! {:state-atom tm.state/state
                             :group (sc.groups/early)
                             :out-fn (fn [_i]
                                       (tm.configs/get-audio-bus
                                        (rand-nth [:arp->nubosidad
                                                   :arp->nubosidad2])))}))

(defn stop-arp!
  []
  (tm.arp/stop-sample-arp!))
(comment
  (start-arp!)
  (stop-arp!))
