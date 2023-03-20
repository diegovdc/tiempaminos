(ns tieminos.habitat.parts.noche
  (:require
   [clojure.core.async :as a]
   [helins.interval.map :as imap]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.panners :refer [panner stop-panner!]]
   [tieminos.habitat.parts.amanecer :refer [quick-jump-trayectories]]
   [tieminos.habitat.parts.dia-back :refer [make-wave-emisions-refrain]]
   [tieminos.habitat.routing :refer [texto-sonoro-rand-mixer-bus]]
   [tieminos.habitat.synths.convolution
    :refer [live-convolver live-convolver-perc]]
   [tieminos.sc-utils.ctl.v1 :refer [ctl-interpolation]]
   [tieminos.utils :refer [ctl-synth rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn de-la-montana-al-fuego
  [context]
  (timbre/info "de-la-montana-al-fuego")
  (let [{:keys [dur-s inputs preouts]} @context]
    (doseq [[k {:keys [bus]}] inputs]
      (panner
       {:in bus
        :out (:bus (k @preouts))
        :type :trayectory
        :trayectory (quick-jump-trayectories dur-s
                                             {#(rrange 4 8) 1
                                              #(rrange 8 12) 2}
                                             :min-width 1.3
                                             :max-width 4)}))))

(defn fuego-conv-synth
  [{:keys [in1 in2 dur out]}]
  (let [a (* dur 0.2)
        s (* dur 0.5)
        r (max 3 (* dur 0.3))
        amp (rrange 1.4 2.4)]
    (timbre/debug "fuego-conv-synth" {:in1 (:name in1)
                                      :amp amp
                                      :dur (float dur)})
    (live-convolver-perc
     {:group (groups/mid)
      :in1 in1
      :in1-amp 1
      :delay (rand-nth [0 (rand)])
      :in2 in2
      :in2-amp 1
      :amp amp
      :amp-lfo-freq (rrange 0.03 1)
      :amp-lfo-min 0.6
      :amp-lfo-max 1
      :a a
      :s s
      :r r
      :curve 1
      :max-amp 0.9
      :bpf-amp (rrange 0.7 1.2)
      :bpf-rev-amp 1.5
      :hpf-freq 100
      :lpf-freq 20000
      :rev-mix 0.7
      :out out})))

(do
  (defn- fuego-conv-probability-pattern
    [total-events]
    (let [probability-map (-> imap/empty
                              (imap/mark  0 0.1 #(> 0.2 (rand)))
                              (imap/mark  0.1 0.2 #(> 0.3 (rand)))
                              (imap/mark  0.2 0.3 #(> 0.4 (rand)))
                              (imap/mark  0.3 0.5 #(> 0.5 (rand)))
                              (imap/mark  0.5 0.6 #(> 0.6 (rand)))
                              (imap/mark  0.6 0.7 #(> 0.7 (rand)))
                              (imap/mark  0.7 0.8 #(> 0.5 (rand)))
                              (imap/mark  0.8 0.9 #(> 0.4 (rand)))
                              (imap/mark  0.9 0.95 #(> 0.1 (rand)))
                              (imap/mark  0.95 1 (constantly false)))]
      (->> total-events
           range
           (map (fn [i]
                  (let [f (first (probability-map (/ (inc i) total-events)))]
                    (when f
                      (f)))))
           (remove nil?)
           (partition-by identity)
           (map (juxt first count)))))
  (fuego-conv-probability-pattern 50))

(defn- make-conv-refrains
  [total-events panner-buses context]
  (def panner-buses panner-buses)
  (let [{:keys [dur-s inputs special-inputs reaper-returns main-fx]} @context
        texto-sonoro-bus (:bus (:texto-sonoro special-inputs))]
    (mapv
     (fn [[k {:keys [bus]}]]
       (let [probability-pattern (fuego-conv-probability-pattern total-events)
             pattern-durs (map second probability-pattern)
             dur-ratio (/ dur-s total-events)
             durs (map #(* % dur-ratio) pattern-durs)
             id (keyword "noche" (str "fuego-" k))]
         (ref-rain
          :id id
          :loop? false
          :durs durs
          :on-event (on-event
                     (let [[play? pattern-dur] (nth probability-pattern i [false])]
                       (when play?
                         (fuego-conv-synth {:in1 bus
                                            ;; FIXME the require for this
                                            :in2 texto-sonoro-rand-mixer-bus
                                            :dur (* 2 dur-ratio pattern-dur)
                                            :out (rand-nth panner-buses)})))))
         id))
      ;; TODO revisar inputs que desea Milo
     inputs
     #_(select-keys inputs [:guitar :mic-1 :mic-4 :mic-5]))))

(defn fuego
  [context]
  (timbre/info "fuego")
  ;; TODO probar intensivamente fuego para garantizar que no crashea al servidor :S
  (let [{:keys [inputs preouts main-fx]} @context
        total-events 100
        panner-buses (mapv (fn [i]
                             (let [panner-bus (o/audio-bus 1 (str "noche-fuego-convolution-out-" i))]
                               (panner
                                {:type :rand
                                 :in panner-bus
                                 :out (:bus (:mid-reverb @main-fx))
                                 :rate (rrange 0.3 0.7)})
                               panner-bus))
                           (range 5))
        ;; TODO revisar que esto esté funcionando bien, en la grabación no hay nada en el return 3
        conv-refrain-ids (make-conv-refrains total-events panner-buses context)]
    (doseq [[k {:keys [bus]}] inputs]
      (panner {:type :rand
               :in bus
               :out (:bus (k @preouts))
               :rate (rrange 0.1 0.3)
               :width 2.7}))
    (swap! context assoc :noche/fuego {:panner-buses panner-buses
                                       :refrain-ids conv-refrain-ids})))

(defn fuego-stop
  [context]
  ;; TODO
  (timbre/info "fuego-stop")
  (let [{:keys [panner-buses refrain-ids]} (:noche/fuego @context)]
    (a/go
      (a/<! (a/timeout 10000))
      (doseq [id refrain-ids]
        (gp/stop id))
      (a/<! (a/timeout 5000))
      (doseq [bus panner-buses]
        (stop-panner! bus))
      (a/<! (a/timeout 10000))
      (doseq [bus panner-buses]
        (o/free-bus bus)))))
(comment
  (-> gp/refrains deref keys))
(defn polinizadores-nocturnos
  "Resonancias que se esparcen por el espacio. Dura aprox 6.5 minutos

  6 olas de señal color-olor:
  - Cada ola
    - guitar y perc -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - emision/multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
  "
  [context]
  (fuego-stop context)
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
    (assoc-refrain-to-context context wave-refrain-ids)
    (doseq [[k {:keys [bus]}] (select-keys inputs [:guitar :mic-1 :mic-2])]
      (let [refrain-id (main-refrain-id-fn (name k))]
        (make-wave-emisions-refrain
         {:refrain-id refrain-id
          :refrain-durs (repeat total-waves wave-dur)
          :wave-dur dur*
          :multiplier-refrain-id-fn (fn [index]
                                      (keyword "noche" (format "polinizadores-emision-de-señal-wave-multiplier-%s%s" (name k) index)))
          :input bus
          :texto-sonoro-input texto-sonoro-rand-mixer-bus
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
  Sonido convolucionado con la cinta crece en presencia y tiene un movimiento aleatorio.
  Aumenta reverb"
  [context]
  (timbre/info "hacia-un-nuevo-universo")
  (let [{:keys [dur-s inputs preouts reaper-returns special-inputs main-fx]} @context
        convolver-synths (mapv (fn [pos [k {:keys [bus]}]]
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
                                   (let [end-1 (if (= k :guitar) 1.5 0.85)
                                         end-2 (if (= k :guitar) 3 1)]
                                     (ctl-interpolation {:dur-s (/ dur-s 3)
                                                         :step-ms 100
                                                         :synth convolver
                                                         :params {:amp {:start 0 :end end-1}}})
                                     (ctl-interpolation {:delay-s (+ 5 (/ dur-s 3))
                                                         :dur-s (/ dur-s 3)
                                                         :step-ms 100
                                                         :synth convolver
                                                         :params {:amp {:start end-1 :end end-2}}}))
                                   convolver))
                               (range 0 2 1/4)
                               (shuffle (into [] inputs)))]

    (doseq [[i [k {:keys [bus]}]] (map-indexed vector inputs)]
      (let [pos (* i 1/4)]
        (timbre/debug "pos size" pos k bus)
        (panner {:in bus
                 :type :trayectory
                 :out (:bus ((if (= k :guitar) :osc-reverb :light-reverb)
                             @main-fx))
                 :trayectory [{:pos pos :width 1.3 :dur (/ dur-s 4)}
                              {:pos (+ pos 1.25) :width 2.5 :dur (/ dur-s 4)}
                              {:pos (+ pos 2.5) :width 4 :dur (/ dur-s 8)}
                              {:pos (+ pos 3) :width 3 :dur (/ dur-s 4)}
                              {:pos (+ pos 3.25) :width 4 :dur (/ dur-s 4)}]
                 :a 7})))
    (ctl-interpolation {:dur-s (* 2/3 dur-s)
                        :step-ms 100
                        :synth (:synth (:osc-reverb @main-fx))
                        :params {:amp {:start 1 :end 3}}})
    (ctl-interpolation {:dur-s (* 2/3 dur-s)
                        :step-ms 100
                        :synth (:synth (:light-reverb @main-fx))
                        :params {:mix  {:start 0.5 :end 0.7}
                                 :room {:start 0.7 :end 2}
                                 :damp {:start 0.3 :end 0.8}
                                 :amp {:start 1 :end 3}}})
    (ctl-interpolation {:delay (+ 5 (* 2/3 dur-s))
                        :dur-s (* 1/3 dur-s)
                        :step-ms 100
                        :synth (:synth (:light-reverb @main-fx))
                        :params {:mix {:start 0.7 :end 1}
                                 :room {:start 2 :end 4}
                                 :damp {:start 0.8 :end 1}}})
    (swap! context assoc-in [:noche/hacia-un-nuevo-universo :synths] convolver-synths)))

(defn hacia-un-nuevo-universo-stop
  [context]
  (timbre/info "hacia-un-nuevo-universo-stop")
  (let [{:keys [preouts]} @context
        synths (get-in @context [:noche/hacia-un-nuevo-universo :synths])]
    (doseq [s synths]
      (ctl-synth s :gate 0))
    (doseq [[_k {:keys [synth]}] @preouts]
      (ctl-synth synth :release 15 :gate 0))))
