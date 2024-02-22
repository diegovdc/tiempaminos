(ns tieminos.habitat.extended-sections.polinizadores-nocturnos.emisiones-refrain
  "Refrains and synths taken originally from `tieminos.habitat.parts.noche` and references, git hash: `6ba6788`"
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.parts.dia-back :refer [make-wave-emisions-refrain]]
   [tieminos.habitat.routing :refer [get-mixed-instrument-return
                                     get-process-instrument-return]]
   [tieminos.utils :refer [rrange]]))

(defn polinizadores-nocturnos
  "Customized version of `noche/polinizadores-nocturnos`.

  Can be called independently of any sequencer:
  ```
  (polinizadores-nocturnos
    (atom (assoc main/context
                 :dur-s 60
                 :polinizadores-nocturnos/total-waves 2
                 ;; prevent overloading the server, but is good for shorter `dur-s`... default is `5000`
                 :polinizadores-nocturnos/wave-emission-call-delay 500)))
  ```

  Resonancias que se esparcen por el espacio. Dura aprox 6.5 minutos

  6 olas de señal color-olor:
  - Cada ola
    - guitar y perc -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - emision/multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
  "
  [context]
  (timbre/info "polinizadores-nocturnos")
  (let  [{:keys [dur-s inputs  texto-sonoro-rand-mixer-bus preouts
                 polinizadores-nocturnos/total-waves
                 polinizadores-nocturnos/wave-emission-call-delay]
          :or {total-waves 6
               wave-emission-call-delay 500}} @context
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
    ;; NOTE An equivalent function should be called somewhere else
    #_(doseq [[k {:keys [bus]}] @inputs]
        (panner {:type :rand
                 :in bus
                 :out (:bus (k @preouts))
                 :rate (rrange 0.1 0.3)
                 :width 2.7}))
    (timbre/info "WECD" wave-emission-call-delay)
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
           :context context
           :call-delay wave-emission-call-delay})
        (assoc-refrain-to-context context [refrain-id])))))
