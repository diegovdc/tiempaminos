(ns tieminos.habitat.parts.dia
  (:require
   [erv.mos.mos :as mos]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.panners :refer [panner panner-rate stop-panner!]]
   [tieminos.habitat.parts.amanecer :refer [make-convolver-1]]
   [tieminos.habitat.utils :refer [rand-time-segments]]
   [tieminos.math.random-walk :refer [rand-walk1]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo lfo-kr]]
   [tieminos.utils :refer [iter-async-call rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

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
        main-amp-env (o/envelope [0 1 1 1 0.3 0.7 0]
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
                                         (* 2 (o/env-gen conv-amp-env)))
                     full-synth (-> (+ convolver-synth
                                       (* guitar-synth (o/env-gen main-amp-env)))
                                    (* (o/env-gen (o/envelope [0 1 1 0]
                                                              [0.5 dur 0.5])
                                                  :action o/FREE))
                                    (o/limiter 0.9 0.05))]
                 (o/out main-out full-synth)
                 (o/out multiplier-out (o/mix full-synth))))]
    (synth (groups/mid))))

(comment
  (oe/defsynth dili [dur 2
                     delay 2]
    (o/out 0
           (* (o/env-gen (o/envelope [0 1 1 0]
                                     [0.01 (+ dur delay) 0.01]) :action o/FREE)
              (o/delay-n (* 1 (o/sin-osc 200)
                            (o/env-gen (o/env-perc)))
                         delay
                         delay))))
  (dili {:delay 0}))

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
   delay 0]
  (o/out out
         (let [filter-rq-lfo (lfo (o/rand 0.2 4) 0.1 0.7)
               sig           (-> (o/in in)
                                 (* amp
                                    (lfo (o/rand 0.3 2) 0.3 1)
                                    (o/env-gen (o/envelope [0 1 0] [a dur])))
                                 (o/delay-n delay delay)
                                 (o/free-verb (o/rand 0.5 1) (o/rand 0.8 1.5) (o/rand 0.2 0.8))
                                 (o/rhpf (+ hp-freq
                                            (lfo (o/rand 0.1 0.8) hp-freq-lfo-min hp-freq-lfo-max))
                                         filter-rq-lfo)
                                 (o/rlpf (+ lp-freq
                                            (lfo (o/rand 0.5 0.8) lp-freq-lfo-min lp-freq-lfo-max))
                                         filter-rq-lfo)
                                 (* 20
                                    (o/amp-comp-a (/ (+ hp-freq lp-freq) 2))
                                    (o/env-gen (o/envelope [0 1 1 0] [0.001 (+ a dur d delay) 0.001])
                                               :action o/FREE)))]
           (oe/circle-az :num-channels 4
                         :in sig
                         :pos pos
                         :width width
                         :orientation 0))))

(defn make-alejamientos-señal-synth
  [{:keys [in out delay] :or {delay 0}}]
  (let [hp-freq (rrange 200 5000)
        hp-freq-lfo-freq-amp (/ hp-freq 100)
        lp-freq (* hp-freq (rrange 1.2 3))
        lp-freq-lfo-freq-amp (/ lp-freq 100)
        a (rrange 0.6 3)
        dur (* a (rrange 0.8 3))]
    (when (> (rand) 0.3) 3
          (alejamientos-senal-synth
           {:group (groups/mid :tail)
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
            :width (rrange 1.2 2.5)}))))

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
  (o/kill ffss)
  (ref-rain
   :id :alejamiento-flor-test
   :durs [1]
   :loop? false
   :on-event (on-event
              (println "holas")
              #_(println :alejamiento-flor-test)
              #_(make-alejamientos-señal-synth {:in multiplier-out
                                                :out (reaper-returns 3)
                                                :delay (rrange 1  20)})))
  (gp/stop :alejamiento-flor-test))

(defn make-alejamientos-señal-refrain
  [{:keys [dur-s in out delay-fn refrain-id]}]
  (timbre/info "make-alejamientos-señal-refrain" dur-s)
  (ref-rain
   :id refrain-id
   :loop? false
   :durs (repeat (let [n (int dur-s)]
                    ;; prevent empty vector when testing with relatively small event durations
                   (if (>= n 1) n 1))
                 1)
   :on-event (on-event
              (make-alejamientos-señal-synth
               {:in in
                :out out
                :delay (delay-fn)}))))

(defn emision-de-señal-wave
  [{:keys [multiplier-refrain-id
           dur
           guitar-input
           texto-sonoro-input
           main-out
           multiplier-out]}]
  (make-fuente-flor-señal-synth {:dur dur
                                 :guitar-input guitar-input
                                 :texto-sonoro-input texto-sonoro-input
                                 :main-out main-out
                                 :multiplier-out multiplier-out})
  (ref-rain
   :id multiplier-refrain-id
   :durs [(* 0.3 dur) (* 0.3 dur) (* 0.3 dur) (* 0.1 dur)]
   :loop? false
   :on-event (on-event
              (case index
                0 nil
                1 (make-alejamientos-señal-refrain
                   {:refrain-id :dia/alejamientos-señal-1
                    :dur-s dur-s
                    :in multiplier-out
                    :out main-out
                    :delay-fn #(rrange (* 0.1 dur) (* 0.8 dur))})
                2 (make-alejamientos-señal-refrain
                   {:refrain-id :dia/alejamientos-señal-2
                    :dur-s dur-s
                    :in multiplier-out
                    :out main-out
                    :delay-fn #(rrange 2 (* 0.5 dur))})
                3 (make-alejamientos-señal-refrain
                   {:refrain-id :dia/alejamientos-señal-3
                    :dur-s dur-s
                    :in multiplier-out
                    :out main-out
                    :delay-fn #(rrange 0.5 4)})
                (timbre/warn (str multiplier-refrain-id "should have stopped by now"))))))

(defn dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía
  "Comienza Diego.
  Resonancias que se esparcen por el espacio. Dura aprox 2 minutos

  3 olas de señal color-olor:
  - Cada ola
    - guitar -> +width (t0 -> t30% increase width and reverb; t30% -> t60% add convolution; t60% -> t90% multiply and spread invididual subsigals; t90% -> t100% fadeout)
    - multiplication of space-resonance (filtered) + individuality (small sounds, less wide, vibrating differently)
  "
  [context]
  ;; NOTE milo transiciona al dueto
  (timbre/info "dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía")
  (let [{:keys [dur-s inputs special-inputs reaper-returns]} @context
        wave-dur (/ dur-s 3)
        multiplier-out  (o/audio-bus 1 "dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía-multiplier-out")
        assoc-refrain-to-context (fn [context refrain-ids]
                                   (swap! context
                                          update-in [:dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía :refrains]
                                          concat refrain-ids))
        main-refrain-id :dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía]
    (assoc-refrain-to-context context [main-refrain-id
                                       ;; TODO maybe figure out a better way to stop these refrains
                                       :dia/alejamientos-señal-1
                                       :dia/alejamientos-señal-2
                                       :dia/alejamientos-señal-3])
    (ref-rain
     :id main-refrain-id
     :durs [wave-dur wave-dur wave-dur]
     :loop? false
     :on-event (on-event
                (let [multiplier-refrain-id (keyword "dia" (str "emision-de-señal-wave-multiplier" index))]
                  (emision-de-señal-wave
                   {:multiplier-refrain-id multiplier-refrain-id
                    :dur (* 1.3 wave-dur)
                    :guitar-input (:bus (:guitar inputs))
                    :texto-sonoro-input (:bus (:texto-sonoro special-inputs))
                    :main-out (reaper-returns 3)
                    :multiplier-out multiplier-out})
                  (assoc-refrain-to-context context [multiplier-refrain-id]))))))
(do
  (defn- danza-de-trayectorias
    "Movimientos opuestos (180 grados) alternados con trenzados, también alternando con widths opuestos"
    []
    (let [mos (mos/make 55 34)
          oposition-mos (mos 5)
          time-scale 0.1618
          make-oposition-dance (fn [time-scale positions]
                                 (let [one (mapcat (fn [i pos]
                                                     (let [dur (* time-scale (wrap-at i oposition-mos))
                                                           dur-spot (* dur 0.618)
                                                           dur-movement (* dur 0.382)]
                                                       [{:pos pos :dur dur-spot :width 1.3}
                                                        {:pos pos :dur dur-movement :width 1.3}]))
                                                   (range)
                                                   positions)]
                                   [one
                                    (map #(update % :pos dec) one)]))
          interweaving-mos (mos 4)
          interweaving-dance (let [positions (rand-walk1 0.618 16)
                                   weaving-pattern [identity dec identity inc]
                                   [im-s im-L] (-> interweaving-mos set sort)
                                   dance-move (fn [mos-offset weaving-pattern i pos]
                                                (let [mos-point (wrap-at (+ mos-offset i) interweaving-mos)
                                                      dur (* 1.618 time-scale mos-point)
                                                      pos* ((wrap-at i weaving-pattern) pos)
                                                      dur* (* dur 0.618)
                                                      rest* (- dur dur*)]
                                                  (if (= im-L mos-point)
                                                    ;; if `rest*` is L then do a width cresc-dim, else keep width as is
                                                    [{:pos pos* :dur dur* :width 1.3 :im/L? true}
                                                     {:pos pos* :dur (* 0.382 0.5 rest*) :width 1.3}
                                                     {:pos pos* :dur (* 0.618 rest*) :width 3}
                                                     {:pos pos* :dur (* 0.382 0.5 rest*) :width 1.3}]
                                                    [{:pos pos* :dur dur* :width 1.3 :im/L? true}
                                                     {:pos pos* :dur rest* :width 1.3}])))]
                               [(mapcat (partial dance-move 0 weaving-pattern)
                                        (range)
                                        positions)
                                (mapcat (partial dance-move 3 (reverse weaving-pattern))
                                        (range)
                                        positions)])
          oposition-dance-1 (->> (rand-walk1 0.618 26)
                                 (make-oposition-dance (* time-scale 1.618)))
          oposition-dance-2 (->> (rand-walk1 1.618 26)
                                 (make-oposition-dance (* time-scale (* 0.618 1.618))))]

      [(mapcat first
               [oposition-dance-1
                interweaving-dance
                oposition-dance-2])
       (mapcat second
               [oposition-dance-1
                interweaving-dance
                oposition-dance-2])]))
  (->> (danza-de-trayectorias)
       (map #(apply + (map :dur %))))
  (->> (danza-de-trayectorias)
       #_(map #(apply + (map :dur %)))
       first
       (map :dur)
       #_(apply +)))

(defn dueto-con-polinizadores=pt2-percepción-de-señal-danza-desarrollo-de-energía
  "Aquí entra Milo.

  danza-de-trayectorias"
  [context]
  (timbre/info "dueto-con-polinizadores=pt2-percepción-de-señal-danza-desarrollo-de-energía")
  (let [{:keys [dur-s inputs special-inputs preouts]} @context
        [t1 t2] (danza-de-trayectorias)]

    (doseq [[k {:keys [bus]}] inputs]
      (panner
       {:in (:bus (k inputs))
        :type :trayectory
        :trayectory (if (#{:guitar} k) t1 t2)
        :out (:bus (k @preouts))}))))

(defn polen-burst [refrain-id dur-s inputs panner-configs]
  (let [input-buses (vals (select-keys inputs [:guitar :mic-1 :mic-2]))
        polens (+ 8 (rand-int 20))
        avg-dur (/ dur-s polens)
        min-dur (* 0.7 avg-dur)
        max-dur (* 1.3 avg-dur)
        ;; TODO improve durs with bezier curves
        durs* (->> polens
                   (range)
                   (map (fn [_] (max min-dur (rand max-dur)))))]
    (ref-rain
     :id refrain-id
     :durs durs*
     :loop? false
     :on-event (on-event
                  ;; TODO perhaps substitute with a grain synth (using `dust`) and some cps melodicish movement
                (make-convolver-1
                 input-buses
                 (:bus (rand-nth panner-configs))
                 (let [a (rrange 0.3 0.7)]
                   {:delay (rrange 0 (* 10 max-dur))
                    :min-dur 0.2
                    :max-dur 0.5
                    :hpf-freq (rrange 800 1800)
                    :lpf-freq (rrange 2000 10000)
                    :amp (rrange 0.7 1.2)
                    :max-amp 1
                    :a (* a 2/3)
                    :r (* a 4/3)
                    :amp-lfo-freq 100 #_(rrange 0.3 10)}))))))

(defn dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación
  [context]
  (timbre/info "dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación")
  (let [{:keys [dur-s inputs main-fx reaper-returns]} @context
        rand-pan-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt3-rand-pan")
                         :type :rand}
        rand-pan-2-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt3-rand-pan-2")
                           :type :rand}
        circle-l-pan-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt3-circle-l-pan")
                             :type :counter-clockwise}
        circle-l-pan-2-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt3-circle-l-pan-2")
                               :type :counter-clockwise}
        circle-r-pan-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt3-circle-r-pan")
                             :type :clockwise}
        circle-r-pan-2-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt3-circle-r-pan-2")
                               :type :clockwise}
        panner-configs [rand-pan-config rand-pan-2-config
                        circle-l-pan-config circle-l-pan-2-config
                        circle-r-pan-config circle-r-pan-2-config]
        durs (rand-time-segments dur-s {#(rrange 3 5) 2
                                        #(rrange 2 3) 1})
        polen-bursts-refrain :dia/polen-bursts]

    (doseq [{:keys [bus type out]} panner-configs]
      (panner
       {:in bus
        :type type
        :out (:bus (:osc-reverb @main-fx))})
      (panner-rate {:in bus
                    :rate (rrange 0.02 0.4)}))
    (ref-rain
     :id polen-bursts-refrain
     :durs durs
     :loop? false
     :on-event (on-event
                (when (> (rand) 0.3)
                  (let [refrain-id (keyword "dia" (str "polen-burst-" index))]
                    (polen-burst index dur-s inputs panner-configs)

                    (swap! context update-in
                           [:dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación
                            :refrain-ids]
                           conj refrain-id)))))
    (swap! context
           (fn [ctx]
             (-> ctx
                 (assoc-in
                  [:dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación
                   :panner-buses]
                  (map :bus panner-configs))
                 (update-in
                  [:dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación
                   :refrain-ids]
                  conj
                  polen-bursts-refrain))))))

(defn dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop
  [context]
  (timbre/info "dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop")
  (let [k :dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación
        {:keys [panner-buses refrain-ids]} (k @context)]
    (ref-rain
     :id :dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop
     :durs [5 10 5 1]
     :loop? false
     :on-event (on-event
                (swap! context dissoc k)
                (case index
                  0 (do (doseq [id refrain-ids]
                          (gp/stop id)))
                  1 (doseq [bus panner-buses]
                      (stop-panner! bus))
                  2 (doseq [bus panner-buses]
                      (o/free-bus bus))
                  3 (timbre/info "dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop done")
                  (timbre/info "dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop should have stopped by now"))))))

(oe/defsynth
  orbital-distributor
  [in 0
   gate 1
   release 5
   out1 0 out2 1 out3 2 out4 3]
  (let [env (o/env-gen (o/env-adsr 3 1 1 release :curve [1 -0.5 -0.5])
                       gate
                       :action o/FREE)
        [a b c d] (* env (oe/circle-az :num-channels 4
                                       :in (o/in  in)
                                       :pos (lfo-kr 0.5 0 2)
                                       :width 1.1
                                       :orientation 0))]

    (o/out out1 a)
    (o/out out2 b)
    (o/out out3 c)
    (o/out out4 d)))

(comment
  (def b (o/audio-bus 1))
  (oe/defsynth sini [out 0]
    (o/out out (* 0.2 (o/sin-osc 200))))

  (sini {:out b})
  (orbital-distributor {:in b})
  (o/stop))

(defn dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales
  [context]
  ;; TODO improve with other panning patterns, like a simple pan-az
  (timbre/info "dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales")
  (let [{:keys [dur-s inputs main-fx reaper-returns current-panners]} @context
        rand-pan-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt4-rand-pan")
                         :type :rand
                         :rate 0.1
                         :out (:bus (:osc-reverb @main-fx))}
        rand-pan-2-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt4-rand-pan-2")
                           :type :rand
                           :rate 0.2
                           :out (:bus (:mid-reverb @main-fx))}
        circle-l-pan-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt4-circle-l-pan")
                             :type :counter-clockwise
                             :rate 0.3
                             :out (:bus (:osc-reverb @main-fx))}
        circle-l-pan-2-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt4-circle-l-pan-2")
                               :type :counter-clockwise
                               :rate 0.4
                               :out (:bus (:mid-reverb @main-fx))}
        circle-r-pan-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt4-circle-r-pan")
                             :type :clockwise
                             :rate 0.33
                             :out (:bus (:osc-reverb @main-fx))}
        circle-r-pan-2-config {:bus (o/audio-bus 1 "dueto-polinizadores-pt4-circle-r-pan-2")
                               :type :clockwise
                               :rate 0.4
                               :out (:bus (:mid-reverb @main-fx))}
        panner-configs [#_#_rand-pan-config rand-pan-2-config
                        circle-l-pan-config circle-l-pan-2-config
                        circle-r-pan-config circle-r-pan-2-config]
        #_#_durs (rand-time-segments dur-s {#(rrange 3 5) 2
                                            #(rrange 2 3) 1})
        ;; polen-bursts-refrain :dia/polen-bursts
        orbital-distributor-synths (mapv (fn [[k {:keys [bus]}]]
                                           (orbital-distributor
                                            {:group (groups/mid)
                                             :in bus
                                             :out1 (:bus circle-l-pan-config)
                                             :out2 (:bus circle-l-pan-2-config)
                                             :out3 (:bus circle-r-pan-2-config)
                                             :out4 (:bus circle-r-pan-config)}))
                                         inputs)
        ;; NOTE this `mapv` will initialize the panners
        panner-buses-and-async-ctl-fns (mapv (fn [{:keys [bus type out rate]}]
                                               (let [width 1.2]
                                                 (panner
                                                  {:in bus
                                                   :type type
                                                   :out out
                                                   :width width})
                                                 (panner-rate {:in bus
                                                               :rate rate})
                                                 (let [interval-ms 200
                                                       end-rate (rrange 1.2 2.5)
                                                       rate-increment (/ (- end-rate rate)
                                                                         (/ (* dur-s 1000)
                                                                            interval-ms))
                                                       max-width 3.5
                                                       width-increment (/ (- max-width width)
                                                                          (/ (* dur-s 1000)
                                                                             interval-ms))]
                                                   {:bus bus
                                                    :stop-fn (iter-async-call interval-ms
                                                                              (fn [i]
                                                                                (let [panner (:synth (get @current-panners bus))
                                                                                      new-rate (+ rate (* rate-increment i))
                                                                                      new-width (+ width (* width-increment i))]
                                                                                  (when-not (> new-rate end-rate)
                                                                                    (o/ctl panner :rate new-rate))
                                                                                  (when-not (> new-width max-width)
                                                                                    (o/ctl panner :width new-width)))))})))
                                             panner-configs)]

    (swap! context
           (fn [ctx]
             (-> ctx
                 (assoc-in
                  [:dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales
                   :panner-buses]
                  (map :bus panner-configs))
                 (update-in
                  [:dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales
                   :synths]
                  concat
                  orbital-distributor-synths)
                 (assoc-in
                  [:dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales
                   :panner-buses-and-async-ctl-fns]
                  panner-buses-and-async-ctl-fns))))))

(defn dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop
  [context]
  (timbre/info "dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop")
  (let [k :dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales
        {:keys [panner-buses synths panner-buses-and-async-ctl-fns]} (k @context)]
    (ref-rain
     :id :dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop
     :durs (mapv #(* % (get @context :stop-rate 1))
                 [5 10 5 1])
     :loop? false
     :on-event (on-event
                (swap! context dissoc k)
                (case index
                  0 (do
                      (doseq [f panner-buses-and-async-ctl-fns]
                        (f))
                      (doseq [synth synths]
                        (o/ctl synth :gate 0)))
                  1 (doseq [bus panner-buses]
                      (stop-panner! bus))
                  2 (doseq [bus panner-buses]
                      (o/free-bus bus))
                  3 (timbre/info "dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop done")
                  (timbre/info "dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop should have stopped by now"))))))

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
