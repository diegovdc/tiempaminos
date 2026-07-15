(ns tieminos.tierra-mar.v1.lluvia

  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :refer [deg->freq]]
   [overtone.core :as o]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.compositions.garden-earth.base :refer [eik]]
   [tieminos.math.utils :refer [normalize]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [tieminos.tierra-mar.v1.synths :refer [+outs1 panaz-line]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

(defonce ^:private state (atom {}))

;; * Lluvia: presentación de la voz
;; 1. voz del planeta, cadena de la vida vidagua: voz melodiza
;; 2. poco a poco la voz se estira y se mueve (az-widths)
;; 3. del centro de la voz vuelve a comenzar a salir la flauta en espirales (quizá "duplicada" por sintetizadores o pitch-shifts o algo)

;;;;;;;;;;;;;;;;;;
;; * Voice paths
;;;;;;;;;;;;;;;;;;

(->> (range 1 26)
     (partition 8 8 nil))

(def ^:private levels
  (->> (range 1 26)
       (partition 8 8 nil)
       reverse))

(defn bound-path-value
  [prev-val direction min-of-level max-of-level]
  (let [x (+ direction prev-val)]

    (cond
      (< x min-of-level) max-of-level
      (> x max-of-level) min-of-level
      :else x)))

#_(reduce (fn [acc _]
            (conj acc (bound-path-value (last acc) 1 9 16)))
          [9]
          (range 1 12))

(defn- make-voice-path
  [len]
  (:path (reduce (fn [{:keys [path level direction]
                       :as acc} _]
                   (let [prev-val (last path)
                         outer-ring? (> 9 prev-val)
                         next-level? (and (not outer-ring?)
                                          (> (rand) 0.6))
                         level* (nth levels level)
                         max-of-level (last level*)
                         min-of-level (first level*)]
                     (cond
                       (zero? level)
                       (let [x (-> levels (nth 1) rand-nth)]
                         (-> acc
                             (update :path (fn [xs] (conj xs x)))
                             (update :level inc)))
                       next-level?
                       (-> acc
                           (update :path (fn [xs]  (conj xs (- prev-val 8))))
                           (update :level inc)
                           #_(assoc :direction (rand-nth [-1 1])))

                       :else
                       (update acc :path (fn [xs] (conj xs
                                                        (bound-path-value
                                                         prev-val
                                                         direction
                                                         min-of-level
                                                         max-of-level)))))))
                 {:path [25]
                  :direction (rand-nth [-1 #_1])
                  :level 0}
                 (range len))))
#_(make-voice-path (rrand 4 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Lorentizian Flows (OSC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn send-lorentzian-flow-osc
  [iem-osc-client lorentz-system i]
  (let [i* (* i)
        az (lorentz/bound (lorentz-system i*) :x -60 60)
        el (lorentz/bound (lorentz-system i*) :y 0 60)
        roll (lorentz/bound (lorentz-system i*) :z -60 60)]
    (osc/osc-send iem-osc-client "/MultiEcoder/masterAzimuth" (float az))
    (osc/osc-send iem-osc-client "/MultiEncoder/masterElevation" (float el))
    (osc/osc-send iem-osc-client "/MultiEncoder/masterRoll"  (float roll))))

(defn send-lorentzian-shadow-osc
  [iem-osc-client lorentz-system i
   & {:keys [az-range el-range roll-range]
      :or {az-range [-60 60]
           el-range [0 60]
           roll-range [-60 60]}}]
  (let [i* (* i)
        [az1 az2] az-range
        [el1 el2] el-range
        [roll1 roll2] roll-range
        az (lorentz/bound (lorentz-system i*) :x az1 az2)
        el (lorentz/bound (lorentz-system i*) :y el1 el2)
        roll (lorentz/bound (lorentz-system i*) :z roll1 roll2)]
    (osc/osc-send iem-osc-client "/StereoEncoder/azimuth" (float az))
    (osc/osc-send iem-osc-client "/StereoEncoder/elevation" (float el))
    (osc/osc-send iem-osc-client "/StereoEncoder/roll"  (float roll))
    (osc/osc-send iem-osc-client "/StereoEncoder/width"  (float (* 2 (+ az el roll))))))

(defn start-lorentzian-osc!
  "OSC for the multi encoders"
  []
  (let [lor (lorentz/init-system :x 0.91 :y 0.06 :z 0.02 :dt 0.005)
        lor2 (lorentz/init-system :x 0.81 :y 0.06 :z 0.02 :dt 0.03)]
    (rain.v2/ref-rain
     :id ::lorentzian-encoders
     :durs [0.3]
     :on-event
     (rain.v2/on-event
      (send-lorentzian-flow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-voz)
       lor (+ 1000 i))
      (send-lorentzian-flow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-fl)
       lor (+ 1000 i))

      (send-lorentzian-shadow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-voz-shadow)
       lor2 (+ 1000 i))
      (send-lorentzian-shadow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-fl-shadow)
       lor2 (+ 1000 i)
       {:az-range [60 -60]
        :roll-range [60 -60]
        :el-range [-60 0]})))))
(comment
  (start-lorentzian-osc!)
  (rain.v2/stop))

#_(make-voice-path 8)

;;;;;;;;;;;;;;;;;;
;; * Canto-lluvia
;;;;;;;;;;;;;;;;;;

(oe/defsynth voice-center
  [in 0
   amp 1
   gate 1
   r 1
   out 25]
  (let [sig-o (-> in
                  (o/sound-in))
        del 0.1
        sig-filtered (-> sig-o
                         (o/moog-ladder [400 1000 4000 7000] 0.7)
                         (o/mix)
                         (* 5))

        sig (+ (* 0.7 sig-o) sig-filtered)]
    (o/out out (->
                (+ sig)
                (* amp
                   (o/env-gen (o/env-adsr 0.5 1 1 r)
                              :gate gate
                              :action o/FREE))))))

(comment
  ;; usado abajo
  lfo-kr)
(make-synth-fn
 'flowering
 (-> {:in 0
      :filter-freq 8000
      :filter-q 0.8
      :delay [0.5 0.5]
      :bpf-freqs [900 1000 2400 8000]
      :delay-dcy 1
      :amp 0.5
      :dur 2
      :asr [0.1 0.65 0.25]
      :curve 0
      :rev-mix 0.7
      :rev-room 1}
     panaz-line
     +outs1)
 '(let [sig (o/sound-in in)
        filter-time-scale (* (o/rand 0.5 1.2) dur)]
    (-> sig
        (o/bpf bpf-freqs
               (o/env-gen:kr (o/envelope [0.4 0.01 0.1 0.4]
                                         asr)
                             :time-scale filter-time-scale))

        (o/mix)
        (* 1.5 #_(o/env-gen:kr (o/envelope [2 8 8 2] asr)
                               :time-scale filter-time-scale))
        (+ (* 0.6 sig))
        (:ugen/panner)
        (o/free-verb rev-mix rev-room 0.3)
        (#(+ % (-> %
                   (o/comb-l delay delay delay-dcy)
                   (o/moog-ladder 10800 0.3)
                   (* (lfo-kr (o/rand 0.3 1) 1.4 4)))))
        (* amp
           (o/env-gen:kr
            (o/envelope [0 1 1 0] asr curve)
            :time-scale dur
            :action o/FREE))
        (:ugen/outs)))
 {:reset? true})

(defn trigger-flowering!
  [dur outs]
  (flowering
   {:in (tm.configs/get-input :voz-main)
    :dur dur
    :amp (* 0.6 (rrand 0.5 0.8))
    :delay (rrand 0.1 1)
    :delay-dcy (rrand 1 3.0)
    :asr (normalize [1 2 2])
    ;; :min-width 1
    :bpf-freqs (map #(float (deg->freq (:scale eik) (* 2 440) %))
                    (repeatedly 6  #(rand-int 60)))
    :pan-dur-till-last (rrand 0.2 0.9)
    :rev-room (rrand 0.6 1.2)
    :max-width (rrand 3 5.0)
    :out-offset (dec (tm.configs/get-output :lluvia-voice-dome-25ch))
    :outs outs}))

#_(make-voice-path (rrand 4 10))

(defn start-flowering-rain!
  []
  (timbre/info "Starting: `::flowering-rain`")
  (let [dur-weights {#(rrand 3 5) 1
                     #(rrand 2 3) 6
                     #(rrand 1 2) 2
                     ;; #(rrand 0.3 1) 1
                     }]
    (rain.v2/ref-rain
     :id ::flowering-rain
     :durs (fn [_] (weighted dur-weights))
     :on-event (rain.v2/on-event
                (trigger-flowering!
                 (rrand  (* 2 dur-s) (*  5 dur-s))
                    ;; TODO: make it so that paths grow (statistically) larger as the section unfolds
                 #_[25 18 17 9 16 15 7]
                 (make-voice-path (rrand 4 10)))))))

(defn stop-flowering-rain!
  []
  (timbre/info "Stopping: `::flowering-rain`")
  (rain.v2/stop ::flowering-rain))

(defn start-cantolluvia!
  "Keeps the voice at the center (ch. 25 of the `:lluvia-voice-dome-25ch`)
  and also makes it grow in downward paths."
  [section-atom]
  (timbre/info "Starting: `::voice-center-synth`")
  (let [synth (voice-center {:in (tm.configs/get-input :voz-main)
                             :amp 0.3
                             :out (+ 24 (tm.configs/get-output :lluvia-voice-dome-25ch))})]
    (swap! section-atom assoc ::voice-center-synth synth)
    (start-flowering-rain!)))

(defn stop-cantolluvia!
  [section-atom]
  (when-let [synth (::voice-center-synth @section-atom)]
    (timbre/info "Stopping: `::voice-center-synth`")
    (o/ctl synth :gate 0)
    (swap! section-atom dissoc ::voice-center-synth))
  (stop-flowering-rain!))

(defn restart-cantolluvia!
  [state]
  (stop-cantolluvia! state)
  (start-cantolluvia! state))

(comment
  (start-cantolluvia! state)
  (stop-cantolluvia! state))

;;;;;;;;;;;;;;;;;;
;; * flute-rain
;;;;;;;;;;;;;;;;;;

(comment

  ;; make straight downward paths for the flute
  (let
   [in 0
    amp 1
       ;; delay/attack/sustain/release: the delay can serve to hide the drop's attack
    dasr (normalize [0.1 1 1 1])
    curve [-1 4]
    dur 2
    drop-ar [0.05 0.2]
    ps-ratio 1
    ps-mix 0.5
    sound (o/sound-in in)
    droplet (* sound (o/env-gen (o/env-perc (first drop-ar)
                                            (second drop-ar))))
    fall (o/free-verb droplet 1 1 0)
    fall-orig (* fall (- 1 (max 1 (abs ps-mix))))
    fall-ps (-> fall
                (o/pitch-shift 0.5 ps-ratio)
                (* ps-mix))]
    (-> (+ fall-orig fall-ps)
          ;; TODO: some resonant filter might be nice, perhaps an hpf or bpf
        (* amp
           (o/env-gen
            (o/envelope [0 0 1 1 0] dasr curve)
            :time-scale dur
            :action o/FREE))
        :ugen/panner
        :ugen/outs)))
