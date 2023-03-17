(ns tieminos.habitat.parts.noche
  (:require
   [helins.interval.map :as imap]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.panners :refer [panner]]
   [tieminos.habitat.parts.amanecer :refer [quick-jump-trayectories]]
   [tieminos.habitat.parts.dia-back :refer [make-wave-emisions-refrain]]
   [tieminos.habitat.synths.convolution
    :refer [live-convolver live-convolver-perc]]
   [tieminos.sc-utils.ctl.v1 :refer [ctl-interpolation]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(defn de-la-montana-al-fuego
  [context]
  (timbre/info "de-la-montana-al-fuego")
  (let [{:keys [dur-s inputs preouts]} @context]
    (panner
     :in (:bus (:guitar inputs))
     :out (:bus (:guitar @preouts))
     :type :trayectory
     :trayectory (quick-jump-trayectories dur-s
                                          {#(rrange 4 8) 1
                                           #(rrange 8 12) 2}
                                          :min-width 1.3
                                          :max-width 3))
    ;; TODO falta Milo
    ))

(defn fuego-conv-synth
  [{:keys [in1 in2 dur out]}]
  (let [a% (rrange 0.1 0.5)
        r% (- 1 a%)]
    (live-convolver-perc
     {:in1 in1
      :in1-amp 1
      :delay 0
      :in2 in2
      :in2-amp 1
      :amp (rrange 0.6 1)
      :amp-lfo-freq (rrange 0.03 16)
      :amp-lfo-min 0.3
      :amp-lfo-max 1
      :a (* dur a%)
      :r (* dur r%)
      :curve -4.0
      :max-amp 0.8
      :bpf-amp (rrange 0.3 0.9)
      :bpf-rev-amp 1.5
      :hpf-freq 500
      :lpf-freq 20000
      :rev-mix 1
      :out out})))

(do
  (defn- fuego-conv-probability-pattern
    [total-events]
    (let [probability-map (-> imap/empty
                              (imap/mark  0 0.3 #(> 0.2 (rand)))
                              (imap/mark  0.4 0.5 #(> 0.5 (rand)))
                              (imap/mark  0.5 0.6 #(> 0.6 (rand)))
                              (imap/mark  0.6 0.7 #(> 0.5 (rand)))
                              (imap/mark  0.7 0.8 #(> 0.3 (rand)))
                              (imap/mark  0.8 0.9 #(> 0.2 (rand)))
                              (imap/mark  0.9 1 #(> 0.1 (rand))))]
      (->> total-events
           range
           (map (fn [i]
                  (let [f (first (probability-map (/ (inc i) total-events)))]
                    (when f
                      (f)))))
           (remove nil?)
           (partition-by identity)
           (map (juxt first count)))))
  (fuego-conv-probability-pattern 100))

(defn- make-conv-refrains
  [total-events panner-buses context]
  (let [{:keys [dur-s inputs special-inputs reaper-returns main-fx]} @context
        texto-sonoro-bus (:bus (:texto-sonoro special-inputs))]
    (mapv
     (fn [[k {:keys [bus]}]]
       (let [probability-pattern (fuego-conv-probability-pattern total-events)
             pattern-durs (map second probability-pattern)
             dur-ratio (/ dur-s total-events)
             durs (map #(* % dur-ratio) pattern-durs)]
         (ref-rain
          :id (keyword "noche" (str "fuego-" k))
          :loop? false
          :durs durs
          :on-event (on-event
                     (let [[play? pattern-dur] (nth probability-pattern i [false])]
                       (when play?
                         (fuego-conv-synth fuego-conv-synth
                                           {:in1 bus
                                            :in2 texto-sonoro-bus
                                            :dur (* dur-ratio pattern-dur)
                                            :out (rand-nth panner-buses)})))))))
     ;; TODO revisar inputs que desea Milo
     (select-keys inputs [:guitar :mic-1 :mic-4 :mic-5]))))

(defn fuego
  [context]
  (timbre/info "fuego")
  ;; TODO probar intensivamente fuego para garantizar que no crashea al servidor :S
  (let [{:keys [main-fx]} @context
        total-events 100
        panner-buses (map (fn [i]
                            (let [panner-bus (o/audio-bus 1 (str "noche-fuego-convolution-out-" i))]
                              (panner
                               :type :rand
                               :in panner-bus
                               :out (:bus (:osc-reverb @main-fx))
                               :rate (rrange 0.3 0.7))
                              panner-bus))
                          (range 5))
        conv-refrains (make-conv-refrains total-events panner-buses context)]))

(defn alejamiento-del-fuego
  [inputs base-preouts]
  (timbre/warn "Not implemented yet: alejamiento-del-fuego"))

(defn polinizadores-nocturnos
  "Resonancias que se esparcen por el espacio. Dura aprox 6.5 minutos

  6 olas de señal color-olor:
  - Cada ola
    - guitar y perc -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - emision/multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
  "
  [context]
  (timbre/info "polinizadores-nocturnos")
  (let  [{:keys [dur-s inputs special-inputs reaper-returns]} @context
         total-waves 6
         wave-dur (/ dur-s total-waves)
         main-out (reaper-returns 3)
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
                           :out main-out
                           :delay-fn delay-fn
                           :should-play-alejamiento?-fn (fn [{:keys [index]}]
                                                          true
                                                          #_(zero? (mod index 2)))
                           :make-synth-config (fn [_]
                                                {:a 0.01
                                                 :amp 0.4
                                                 :dur (rrange 0.4 2)})})
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
    (assoc-refrain-to-context context wave-refrain-ids)
    ;; TODO left here, not sure everything is sounding as expected
    (doseq [[k {:keys [bus]}] (select-keys inputs [:guitar :mic-1 :mic-2])]
      (let [refrain-id (main-refrain-id-fn (name k))]
        (make-wave-emisions-refrain
         {:refrain-id refrain-id
          :refrain-durs (repeat total-waves wave-dur)
          :wave-dur dur*
          :multiplier-refrain-id-fn (fn [index]
                                      (keyword "noche" (format "polinizadores-emision-de-señal-wave-multiplier-%s%s" (name k) index)))
          :input bus
          :texto-sonoro-input (:bus (:texto-sonoro special-inputs))
          :main-out main-out
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

(defn hacia-un-nuevo-universo
  "Sonido directo crece (en `width`) hasta cubrir los 4 canales.
  Sonido convolucionado con la cinta crece en presencia y tiene un movimiento aleatorio. "
  [context]
  (timbre/info "hacia-un-nuevo-universo")
  (let [{:keys [dur-s inputs preouts reaper-returns special-inputs]} @context
        convolver-synths (mapv (fn [pos [k {:keys [bus]}]]
                                 (panner
                                  :in bus
                                  :type :trayectory
                                  :out (:bus (k @preouts))
                                  :trayectory [{:pos pos :width 1.3 :dur (/ dur-s 3)}
                                               {:pos (+ pos 1/2) :width 3.5 :dur (/ dur-s 3)}
                                               {:pos (+ pos 1) :width 4 :dur (/ dur-s 3)}]
                                  :a 7)

                                 (let [convolver (live-convolver
                                                  {:in1 0
                                                   :in1-amp 1
                                                   :in2 (:bus (:texto-sonoro special-inputs))
                                                   :in2-amp 1
                                                   :amp 0
                                                   :release 10
                                                   :gate 1
                                                   :rev-mix 1
                                                   :rev-room 0.2
                                                   :out (reaper-returns 3)})]
                                   (ctl-interpolation {:dur-s (/ dur-s 3)
                                                       :step-ms 100
                                                       :synth convolver
                                                       :params {:amp {:start 0 :end 0.75}}})
                                   (ctl-interpolation {:delay-s (/ dur-s 3)
                                                       :dur-s (/ dur-s 3)
                                                       :step-ms 100
                                                       :synth convolver
                                                       :params {:amp {:start 0.75 :end 0.9}}})
                                   convolver))
                               (range 0 2 1/4)
                               (shuffle (into [] inputs)))]
    (swap! context assoc-in [:noche/hacia-un-nuevo-universo :synths] convolver-synths)))

(defn hacia-un-nuevo-universo-stop
  [context]
  (let [{:keys [preouts]} @context
        synths (get-in @context [:noche/hacia-un-nuevo-universo :synths])]
    (doseq [s synths]
      (o/ctl s :gate 0))
    (doseq [s @preouts]
      (o/ctl s :release 15 :gate 0))))
