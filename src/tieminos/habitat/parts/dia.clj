(ns tieminos.habitat.parts.dia
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as group]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [rrange]]
   [tieminos.habitat.groups :as groups]))

(def seg-dur1 1)
(def seg-dur2 1)
(def seg-dur3 1)
(def seg-dur4 1)

(defn make-fuente-flor-señal-synth
  "See `dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía`.
  Segments: (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)"
  [{:keys [dur guitar-input texto-sonoro-input main-out multiplier-out]}]
  (let [seg-dur1 (* 0.3 dur)       ; t0 -> t30% increase width and reverb;
        seg-dur2 (* 0.3 dur)       ; t30% -> t60% add convolution;
        seg-dur3 (* 0.3 dur)       ; t60% -> t90% multiply and spread individual
        seg-dur4 (* 0.1 dur)       ; t90% -> t100% fadeout
        width-env (o/envelope [1.3 4 4 3 1.3]
                              [seg-dur1
                               seg-dur2
                               seg-dur3
                               seg-dur4])

        pos-env (o/envelope (let [initial-pos (rand)]
                              (->> (range 5)
                                   (map (fn [_] (+ (rrange -0.7 0.7) initial-pos)))
                                   (take 5)
                                   (into [])))
                            [seg-dur1
                             seg-dur2
                             seg-dur3
                             seg-dur4])
        main-amp-env (o/envelope #_[0 0 0 0 0 0] [0 1 1 1 0.3 0.7 0]
                                 [0.5
                                  (- seg-dur1 0.5)
                                  seg-dur2
                                  seg-dur3
                                  (/ seg-dur4 2)
                                  (/ seg-dur4 2)])
        main-reverb-mix-env (o/envelope [0.4 1] [seg-dur1 seg-dur2])
        main-reverb-room-env (o/envelope [0.4 1 2 1] [seg-dur1 seg-dur2 seg-dur3 seg-dur4])
        conv-amp-env (o/envelope [0 1 1 1 0.8 0] #_[0 0 1 1 0.8 0]
                                 [seg-dur1
                                  seg-dur2
                                  seg-dur3
                                  (/ seg-dur4 2)
                                  (/ seg-dur4 2)])
        synth (o/synth
               (let [guitar-synth (-> (oe/circle-az :num-channels 4
                                                    :in (o/in guitar-input)
                                                    :pos (o/env-gen pos-env)
                                                    :width (o/env-gen width-env)
                                                    :orientation 0)
                                      (o/free-verb main-reverb-mix-env main-reverb-room-env))
                     convolver-synth (-> (o/convolution guitar-synth
                                                        (+ (o/delay-n (o/mix guitar-synth) 0.01 0.01)
                                                           (* 0.7 (o/delay-n (o/mix guitar-synth) 0.02 0.02))
                                                           (* 1 (o/in texto-sonoro-input)))
                                                        (/ 4096 2))
                                         (o/hpf 300)
                                         (o/free-verb 0.5 0.2)
                                         (* 2 (o/env-gen conv-amp-env))
                                         (o/limiter 0.8 0.05))
                     full-synth (* (o/env-gen (o/envelope [0 1 1 0]
                                                          [0.5 dur 0.5])
                                              :action o/FREE)
                                   (+ convolver-synth
                                      (* guitar-synth (o/env-gen main-amp-env))))]
                 (o/out main-out full-synth)
                 #_(o/out main-out (o/in texto-sonoro-rand-mixer-bus))
                 (o/out multiplier-out (o/mix full-synth))))]
    (synth (groups/mid))))

(comment
  (require '[tieminos.habitat.routing :refer [inputs texto-sonoro-rand-mixer-bus reaper-returns]]
           '[tieminos.habitat.init :refer [init!]])
  (init!)
  (o/stop)
  (def t ((o/synth (o/out (reaper-returns 3)
                          #_(* 0.2 (o/sin-osc))
                          (o/in texto-sonoro-rand-mixer-bus)))
          (groups/mid)))
  (o/kill t)
  (def multiplier-out (o/audio-bus 1 "multiplier-out"))
  (def ffss (make-fuente-flor-señal-synth {:dur 45
                                           :guitar-input (:bus (:guitar inputs))
                                           :texto-sonoro-input texto-sonoro-rand-mixer-bus
                                           :main-out (reaper-returns 3)
                                           :multiplier-out multiplier-out}))
  (o/kill ffss))

(defn dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía
  "Comienza Diego.
  Resonancias que se esparcen por el espacio. Dura aprox 2 minutos

  3 olas de señal color-olor:
  - Cada ola
    - guitar -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
    - "
  [context]
  ;; NOTE milo transiciona al dueto
  (timbre/info "dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía"))

(defn dueto-con-polinizadores=pt2-percepción-de-señal-danza-desarrollo-de-energía
  "Aquí entra Milo"
  [context]

  (timbre/warn "Not implemented yet: dueto-con-polinizadores-inicio-macro->micro"))

(defn dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación
  [context]

  (timbre/warn "Not implemented yet: dueto-con-polinizadores-inicio-macro->micro"))

(defn dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales
  [context]

  (timbre/warn "Not implemented yet: dueto-con-polinizadores-inicio-macro->micro"))

(defn dueto-con-polinizadores=pt5-movimiento-energía-alejamiento->viento
  [context]

  (timbre/warn "Not implemented yet: dueto-con-polinizadores-inicio-macro->micro"))

(defn tacet-post-dueto-con-polinizadores
  [context]

  (timbre/warn "Not implemented yet: dueto-con-polinizadores-inicio-macro->micro"))

(defn escucha-de-aves
  [inputs base-preouts]
  (doseq [[k {:keys [bus]}] inputs]
    (timbre/warn "Not implemented yet")))
