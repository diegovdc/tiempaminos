(ns tieminos.habitat.extended-sections.polinizadores-nocturnos.emisiones-refrain
  "Refrains and synths taken originally from `tieminos.habitat.parts.noche` and references, git hash: `6ba6788`"
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.panners :refer [panner]]
   [tieminos.habitat.parts.noche :refer [fuego-stop]]
   [tieminos.habitat.routing :refer [get-mixed-instrument-return
                                     get-process-instrument-return]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [rrange sequence-call]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(oe/defsynth fuente-flor-senal
  ;; "See `dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía`.
  ;; Segments: (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)"
  [dur 5
   input 0
   texto-sonoro-input 1
   main-out 0
   multiplier-out 0
   amp 1
   conv-amp 2
   amp-limit 0.9]
  (let [seg-dur1 (* 0.3 dur)       ; t0 -> t30% increase width and reverb;
        seg-dur2 (* 0.3 dur)       ; t30% -> t60% add convolution;
        seg-dur3 (* 0.3 dur)       ; t60% -> t90% multiply and spread individual
        seg-dur4 (* 0.1 dur)       ; t90% -> t100% fadeout
        width-env (o/envelope [1.3 4 4 3 1.3]
                              [seg-dur1
                               seg-dur2
                               seg-dur3
                               seg-dur4])

        pos-env (o/envelope (let [initial-pos (rand 2)]
                              (->> (range 5)
                                   (map (fn [_] (+ (rrange -0.7 0.7) initial-pos)))
                                   (into [])))
                            [seg-dur1
                             seg-dur2
                             seg-dur3
                             seg-dur4])
        main-amp-env (o/envelope [0 1 1 1 0.3 0.7 0]
                                 [0.5
                                  (- seg-dur1 0.5)
                                  seg-dur2
                                  seg-dur3
                                  (/ seg-dur4 2)
                                  (/ seg-dur4 2)])
        main-reverb-mix-env (o/envelope [0.4 1] [seg-dur1 seg-dur2])
        main-reverb-room-env (o/envelope [0.4 1 2 1] [seg-dur1 seg-dur2 seg-dur3 seg-dur4])
        conv-amp-env (o/envelope #_[0 1 1 1 0.8 0] [0 0 1 1 0.8 0]
                                 [seg-dur1
                                  seg-dur2
                                  seg-dur3
                                  (/ seg-dur4 2)
                                  (/ seg-dur4 2)])]
    (let [main-synth (-> (oe/circle-az :num-channels 4
                                       :in (o/in input)
                                       :pos (o/env-gen pos-env)
                                       :width (o/env-gen width-env)
                                       :orientation 0)
                         (o/free-verb main-reverb-mix-env main-reverb-room-env))
          convolver-synth (-> (o/convolution main-synth
                                             (+ (o/delay-n (o/mix main-synth) 0.01 0.01)
                                                (* 0.7 (o/delay-n (o/mix main-synth) 0.02 0.02))
                                                (* 1 (o/in texto-sonoro-input)))
                                             (/ 4096 2))
                              (o/hpf 300)
                              (o/free-verb 0.5 0.2)
                              (* conv-amp (o/env-gen conv-amp-env)))
          full-synth (-> (+ convolver-synth
                            (o/in input)
                            (* main-synth (o/env-gen main-amp-env)))
                         (* amp (o/env-gen (o/envelope [0 1 1 0]
                                                       [0.5 dur 0.5])
                                           :action o/FREE))
                         (o/limiter amp-limit 0.05))]
      (o/out main-out full-synth)
      (o/out multiplier-out (o/mix full-synth)))))

(oe/defsynth alejamientos-senal-synth
  [in 0
   out 0
   dur 3
   amp 1
   pos 1
   width 1.3
   hp-freq 200
   hp-freq-lfo-min 3
   hp-freq-lfo-max 5
   lp-freq 3000
   lp-freq-lfo-min 3
   lp-freq-lfo-max 5
   a 0.1
   d 2
   delay 0
   rev-mix 0.5
   max-amp 1]
  (o/out out
         (let [filter-rq-lfo (lfo (o/rand 0.2 4) 0.1 0.7)
               sig           (-> (o/in in)
                                 (* amp
                                    (lfo (o/rand 0.3 2) 0.3 1)
                                    (o/env-gen (o/envelope [0 1 0] [a (+ d dur)])))
                                 (o/delay-n delay delay)
                                 (o/free-verb rev-mix (o/rand 0.8 1.5) (o/rand 0.2 0.8))
                                 (o/rhpf (+ hp-freq
                                            (lfo (o/rand 0.1 0.8) hp-freq-lfo-min hp-freq-lfo-max))
                                         filter-rq-lfo)
                                 (o/rlpf (+ lp-freq
                                            (lfo (o/rand 0.5 0.8) lp-freq-lfo-min lp-freq-lfo-max))
                                         filter-rq-lfo)
                                 (* 20
                                    (o/amp-comp-a (/ (+ hp-freq lp-freq) 2))
                                    (o/env-gen (o/envelope [0 1 1 0] [0.001 (+ a dur d delay) 5])
                                               :action o/FREE))
                                 (o/limiter max-amp 0.1))]
           (oe/circle-az :num-channels 4
                         :in sig
                         :pos pos
                         :width width
                         :orientation 0))))

(defn make-alejamientos-señal-synth
  [{:keys [in out delay] :or {delay 0} :as config}]
  (let [hp-freq (rrange 200 5000)
        hp-freq-lfo-freq-amp (/ hp-freq 100)
        lp-freq (* hp-freq (rrange 1.2 3))
        lp-freq-lfo-freq-amp (/ lp-freq 100)
        a (rrange 0.6 3)
        dur (* a (rrange 0.8 3))]
    (when (> (rand) 0.3)
      (alejamientos-senal-synth
        (merge {:group (groups/mid :tail)
                :in in
                :out out
                :pos (rrange -1 1)
                :hp-freq hp-freq
                :hp-freq-lfo-min (* -1 hp-freq-lfo-freq-amp)
                :hp-freq-lfo-max hp-freq-lfo-freq-amp
                :lp-freq lp-freq
                :lp-freq-lfo-min (* -1 lp-freq-lfo-freq-amp)
                :lp-freq-lfo-max lp-freq-lfo-freq-amp
                :a a
                :d (* dur (rrange 0.8 1.3))
                :dur dur
                :delay delay
                :amp (rrange 0.4 (if (> a 1.3) 0.7 0.55))
                :width (rrange 1.2 2.5)
                :rev-mix (rrange 0.5 1)}
               config)))))

(defn make-alejamientos-señal-refrain
  [{:keys [dur-s in out delay-fn refrain-id should-play-alejamiento?-fn make-synth-config]
    :or {make-synth-config (fn [_] {})}}]
  (timbre/debug "make-alejamientos-señal-refrain")
  (ref-rain
    :id refrain-id
    :loop? false
    :durs (repeat (let [n (int dur-s)]
                    ;; prevent empty vector when testing with relatively small event durations
                    (if (>= n 1) n 1))
                  1)
    :on-event (on-event
                (when (should-play-alejamiento?-fn {:index index})
                  (let [delay* (delay-fn)]
                    (make-alejamientos-señal-synth
                      (merge {:in in
                              :out out
                              :delay delay*}
                             (make-synth-config {}))))))))

(defn emision-de-señal-wave
  [{:keys [multiplier-refrain-id dur emision-refrain-configs]}]
  (ref-rain
    :id multiplier-refrain-id
    :durs [(* 0.3 dur) (* 0.3 dur) (* 0.3 dur) (* 0.1 dur)]
    :loop? false
    :on-event (on-event
                (let [config (nth emision-refrain-configs index nil)]
                  (when config
                    (make-alejamientos-señal-refrain
                      (assoc config :dur-s dur-s)))
                  (when (> index 3)
                    (timbre/warn (str multiplier-refrain-id "should have stopped by now")))))))

(defn make-wave-emisions-refrain
  [{:keys [refrain-id refrain-durs wave-dur multiplier-refrain-id-fn
           input texto-sonoro-input main-out multiplier-out
           emision-refrain-configs
           make-fuente-flor-señal-synth-params
           assoc-refrain-to-context context]
    :or {make-fuente-flor-señal-synth-params (fn [_] {})}}]
  (ref-rain
    :id refrain-id
    :durs refrain-durs
    :loop? false
    :on-event (on-event
                (let [multiplier-refrain-id (multiplier-refrain-id-fn index)
                      emissions-refrain-fn (fn []
                                             (timbre/debug "emission!" refrain-id index input main-out)
                                             (fuente-flor-senal
                                               (merge {:group (groups/mid)
                                                       :dur wave-dur
                                                       :input input
                                                       :texto-sonoro-input texto-sonoro-input
                                                       :main-out main-out
                                                       :multiplier-out multiplier-out}
                                                      (make-fuente-flor-señal-synth-params {})))
                                             (emision-de-señal-wave
                                               {:multiplier-refrain-id multiplier-refrain-id
                                                :emision-refrain-configs emision-refrain-configs
                                                :dur wave-dur})
                                             (assoc-refrain-to-context context [multiplier-refrain-id]))]
                  (sequence-call 5000 emissions-refrain-fn)))))

(defn polinizadores-nocturnos
  "Resonancias que se esparcen por el espacio. Dura aprox 6.5 minutos

  6 olas de señal color-olor:
  - Cada ola
    - guitar y perc -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - emision/multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
  "
  [context]
  ;; TODO add initialization from `noche/fuego`
  (fuego-stop context)
  (timbre/info "polinizadores-nocturnos")
  (let  [{:keys [dur-s inputs  texto-sonoro-rand-mixer-bus preouts polinizadores-nocturnos/total-waves]
          :or {total-waves 6}} @context
         total-waves total-waves
         wave-dur (/ dur-s total-waves)
         multiplier-out (o/audio-bus 1 "noche/polinizadores-nocturnos")
         assoc-refrain-to-context (fn [context refrain-ids]
                                    (swap! context
                                           update-in [:noche/polinizadores-nocturnos :refrains]
                                           (comp set concat) refrain-ids))
         main-refrain-id-fn (fn [input-name] (keyword "noche" (str "polinizadores-nocturnos-main-" input-name)))
         dur* (* 1.168 wave-dur)
         emision-config (fn [refrain-id delay-fn]
                          {:refrain-id refrain-id
                           :dur-s dur-s
                           :in multiplier-out
                           :out (get-mixed-instrument-return)
                           :delay-fn delay-fn
                           :should-play-alejamiento?-fn (fn [{:keys [_index]}]
                                                          (timbre/debug "playing alejamiento")
                                                          (> (rand) 0.2)
                                                          #_(zero? (mod index 2)))
                           :make-synth-config (fn [_]
                                                (let [dur (rrange 2 5)]
                                                  {:a (* dur (rrange 0.2 0.35))
                                                   :amp (rrange 0.05 0.2)
                                                   :dur dur
                                                   :rev-mix 1
                                                   :max-amp 0.7}))})
         emision-refrain-configs [nil
                                  (emision-config :noche/polinizadores-alejamientos-señal-1
                                                  #(rrange (* 0.1 dur*) (* 0.8 dur*)))
                                  (emision-config :noche/polinizadores-alejamientos-señal-2
                                                  #(rrange 2 (* 0.5 dur*)))
                                  (emision-config :noche/polinizadores-alejamientos-señal-3
                                                  #(rrange 0.5 4))]
         wave-refrain-ids (->> emision-refrain-configs
                               (map :refrain-id)
                               (remove nil?))]
    (doseq [[k {:keys [bus]}] @inputs]
      (panner {:type :rand
               :in bus
               :out (:bus (k @preouts))
               :rate (rrange 0.1 0.3)
               :width 2.7}))
    (assoc-refrain-to-context context wave-refrain-ids)
    (doseq [[k {:keys [bus]}] (select-keys @inputs [:guitar :mic-1 :mic-2 :mic-5 :mic-7])]
      (let [refrain-id (main-refrain-id-fn (name k))]
        (make-wave-emisions-refrain
          {:refrain-id refrain-id
           :refrain-durs (repeat total-waves wave-dur)
           :wave-dur dur*
           :multiplier-refrain-id-fn (fn [index]
                                       (keyword "noche" (format "polinizadores-emision-de-señal-wave-multiplier-%s%s" (name k) index)))
           :input bus
           :texto-sonoro-input @texto-sonoro-rand-mixer-bus
           :main-out (get-process-instrument-return k)
           :multiplier-out multiplier-out
           ;; To prevent creating too many emisions, we just use the `:guitar` ones, but because they all use the `multiplier-out`,
           ;; in theory it will contain the output of all the waves from all the inputs
           :emision-refrain-configs (if (= k :guitar) emision-refrain-configs [])
           :make-fuente-flor-señal-synth-params (fn [_]
                                                  {:amp 0.6
                                                   :conv-amp 1
                                                   :amp-limit 0.6})
           :assoc-refrain-to-context assoc-refrain-to-context
           :context context})
        (assoc-refrain-to-context context [refrain-id])))))
