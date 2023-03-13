(ns tieminos.habitat.parts.noche
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.parts.dia-back :refer [make-wave-emisions-refrain]]
   [tieminos.utils :refer [rrange]]))

(defn de-la-montana-al-fuego
  [inputs base-preouts]
  (timbre/warn "Not implemented yet: de-la-montana-al-fuego"))

(defn fuego
  [inputs base-preouts]
  (timbre/warn "Not implemented yet: fuego"))

(defn alejamiento-del-fuego
  [inputs base-preouts]
  (timbre/warn "Not implemented yet: alejamiento-del-fuego"))

(defn polinizadores-nocturnos
  "Comienza Diego.
  Resonancias que se esparcen por el espacio. Dura aprox 6.5 minutos

  6 olas de señal color-olor:
  - Cada ola
    - guitar -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - emision/multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
  "
  [context]
  ;; NOTE milo transiciona al dueto
  (timbre/info "polinizadores-nocturnos")
  (let [{:keys [dur-s inputs special-inputs reaper-returns]} @context
        total-waves 6
        wave-dur (/ dur-s total-waves)
        main-out (reaper-returns 3)
        multiplier-out (o/audio-bus 1 "noche/polinizadores-nocturnos")
        assoc-refrain-to-context (fn [context refrain-ids]
                                   (swap! context
                                          update-in [:noche/polinizadores-nocturnos :refrains]
                                          (comp set concat) refrain-ids))
        main-refrain-id-fn (fn [input-name] (keyword "noche" (str "polinizadores-nocturnos-main-" input-name)))
        dur* (* 1.3 wave-dur)
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
  [inputs base-preouts]
  (timbre/warn "Not implemented yet: hacia-un-nuevo-universo"))
