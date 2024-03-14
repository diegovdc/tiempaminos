(ns tieminos.habitat.extended-sections.polinizadores-nocturnos.main
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.polinizadores-nocturnos.emisiones-refrain
    :refer [polinizadores-nocturnos]]
   [tieminos.habitat.extended-sections.ui.v1 :refer [add-rec-bar]]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.osc :refer [args->map init responder]]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing
    :refer [guitar-processes-main-out inputs mixed-main-out
            percussion-processes-main-out preouts]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(oe/defsynth sample-player*
  [buf 0
   out 0
   dur 1
   rate 1
   ps-rate 1
   a 0.1
   r 0.1
   amp 1]
  (let [s (max 0 (- dur a r))
        ps-mix (lfo (o/rand 0 1) 0 1)
        sig (o/play-buf :num-channels 1 :bufnum buf :rate rate)]
    (o/out out (-> (+ (* (- 1 ps-mix) sig)
                      (* ps-mix (o/pitch-shift sig 0.2 ps-rate)))
                   (o/pan4 (lfo (o/rand 0 1) -1 1)
                           (lfo (o/rand 0 1) -1 1))
                   (o/free-verb (lfo (o/rand 0 0.4) 0.2 1)
                                (lfo (o/rand 0 2) 0 2))
                   (* amp (o/env-gen (o/envelope [0 1 1 0] [a s r])
                                     :action o/FREE))))))
(defn start-layer-recording!
  [input-k]
  (if-let [bus (:bus (input-k @inputs))]
    (rec/rec-input {:section "polinizadores-nocturnos"
                    :subsection "layer"
                    :input-name (:name bus)
                    :input-bus bus
                    :dur-s 10
                    :countdown 5
                    :on-end (fn [_] (println (format "========== %s Recording done ============"
                                                     (name input-k))))
                    :on-rec-start add-rec-bar})
    (timbre/error "Input not found")))

(defn play-layer!
  [buf]
  (let [out (if (:input-name (:rec/meta buf))
              guitar-processes-main-out
              percussion-processes-main-out)
        rate (weighted {1 20
                        1/2 1
                        2 3
                        2/3 3
                        3/2 3
                        5/4 2
                        11/8 1
                        9/8 1
                        8/9 1
                        7/8 1
                        7/4 1
                        (rrand 0.5 1) 1})
        ps-rate (weighted {1 10
                           3/2 3
                           5/4 2
                           7/4 1
                           7/8 2
                           11/4 2})
        dur (/ (:duration buf) rate)
        amp   (* (rrand 0.01 0.25) (let [amp-norm (:amp-norm-mult buf)]
                                   ;; TODO check and improve
                                     (cond (> amp-norm 5) (/ amp-norm 2)
                                           :else amp-norm)))]
    (-> (sample-player* {:buf buf
                         :dur dur
                         :rate rate
                         :ps-rate ps-rate
                         :a (* 0.2 dur)
                         :r (* 0.4 dur)
                         :amp amp
                         :out out}))))

(do
  ;; FIXME refactor
  (defn start-layers-refrain!
    [bus-name-str]
    (ref-rain
     :id (keyword "layers" bus-name-str)
     :durs (map (fn [_] (rrand 3.0 8))
                (range 100))
     :on-event (on-event
                (when-let [bufs (seq (->> @rec/bufs
                                          vals
                                          (filter #(and (-> % :rec/meta
                                                            ((juxt :input-name :section :subsection))
                                                            (= [bus-name-str "polinizadores-nocturnos" "layer"]))))))]
                  (play-layer! (rand-nth bufs)))))))

(comment
  ;; Older version, tied to a sequencer

  (defn polinizadores-nocturnos*
    [context]
    (noche/polinizadores-nocturnos context))

  (def polinizadores-nocturnos-main
    ;; TODO revisar refrains de emision hay cosas raras (aumentos de volumen y saturación del servidor)
    {:context (merge main/context {})
     :sections [[[52 22] #'polinizadores-nocturnos*]
                [[62 10] (fn [_] (println "end"))]]
     :initial-sections #'polinizadores-nocturnos*
     ;; :rec? true
     })

  (main/start-sequencer! polinizadores-nocturnos-main))

(comment
  (ndef/stop ::flor-base)
  ;; Interior de la flor Ndef
  (let [input-ks [:guitar :mic-1 :mic-2 :mic-3]
        selected-inputs (select-keys @inputs input-ks)
        pan-freq 0.52 ;; 2 works well for the interior, it has a nice beat to it.
        ]
    (ndef/ndef
     ::flor-base
     (->> selected-inputs
          (map (fn  [[k input]]
                 (let [input-bus (:bus input)
                       guitar? (= k :guitar)
                       convolver-input (if guitar?
                                         (->> input-ks (remove :guitar) (map (comp o/in :bus selected-inputs)) o/mix)
                                         (-> selected-inputs :guitar :bus o/in))
                       main-synth (-> (oe/circle-az :num-channels 4
                                                    :in (o/in input-bus)
                                                    :pos (lfo pan-freq -1 1)
                                                    :width (lfo 0.2 1 4)
                                                    :orientation 0)
                                      (o/free-verb (lfo 0.2 0.2 1)
                                                   (lfo 0.2 0.5 1)))
                       convolver-synth (-> (o/convolution main-synth
                                                             ;; TODO test amps
                                                          (+ (* (if guitar? 0 1) (o/delay-n (o/mix main-synth) 0.01 0.01))
                                                             (* 0.7 (o/delay-n (o/mix main-synth) 0.02 0.02))
                                                             (* 1.5 convolver-input))
                                                          (/ 4096 2))
                                           (o/hpf 300)
                                           (o/free-verb 0.5 0.2)
                                           (* 2 (lfo 2 0.5 1)))
                       full-synth (-> (+ convolver-synth
                                         main-synth
                                         (o/free-verb main-synth
                                                      (lfo 2 0.2 1)
                                                      (lfo 2 0.5 3)))
                                      (* (if guitar? 2 3))
                                      (o/limiter 0.8 0.05))]
                   full-synth)))
          o/mix)
     {:out mixed-main-out}))
  :flor-ndef)

(comment
  ;; New version independent of sequencer
  (polinizadores-nocturnos
   (atom (assoc main/context
                :dur-s 60
                :polinizadores-nocturnos/wave-emission-call-delay 500))))

(comment
  (o/demo (* 0.2 (o/sin-osc)))
  (timbre/set-level! :debug)
  (do (when @habitat-initialized?
        (reset! rec/recording? {})
        (reset! rec/bufs {})
        (main/stop-sequencer! hseq/context))
      (init!))

  (start-layer-recording! :guitar)
  (gp/stop)

  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts})

  (do
    (start-layers-refrain! "guitar-bus")
    (start-layers-refrain! "mic-1-bus")
    (start-layers-refrain! "mic-2-bus")
    (start-layers-refrain! "mic-3-bus"))

;; TouchOSC
  (init :port 16180)
  (responder
   (fn [{:keys [path args] :as msg}]
     (let [args-map (args->map args)]
       (case path
         "/rec" (start-layer-recording! (:input args-map))
         (println "Unknown path for message: " msg))))))
