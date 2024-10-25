(ns tieminos.compositions.garden-earth.moments.two.habitat-in-volcanic-temporality
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.base :refer [eik subcps]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp :refer [arp-reponse-2
                                                                     default-interval-seq-fn]]
   [tieminos.compositions.garden-earth.init :as ge.init]
   [tieminos.compositions.garden-earth.moments.two.async-sequencer :as aseq]
   [tieminos.compositions.garden-earth.moments.two.interface :as two.interface]
   [tieminos.compositions.garden-earth.moments.two.live-state :as two.ls]
   [tieminos.compositions.garden-earth.moments.two.rec :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.sections.erupcion :as erupcion]
   [tieminos.compositions.garden-earth.moments.two.sections.fondo-oceanico :as fondo-oceanico]
   [tieminos.compositions.garden-earth.moments.two.sections.formacion-terrestre :as formacion-terrestre]
   [tieminos.compositions.garden-earth.moments.two.sections.totalidad :as totalidad]
   [tieminos.compositions.garden-earth.moments.two.synths :refer [buf-mvts-subterraneos]]
   [tieminos.compositions.garden-earth.routing :refer [fl-i1]]
   [tieminos.habitat.amp-trigger :as amp-trig]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.osc.reaper :as reaper]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.synths :as synths]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))


(defonce analyzers-registry (atom {}))

(defn- merge-analyzers-inputs
  [inputs recordable-outputs]
  (merge (->> inputs (map (fn [[k v]] [(keyword "recordable-input" (name k)) v]))
              (into {}))
         (->> recordable-outputs (map (fn [[k v]] [(keyword "recordable-output" (name k)) v]))
              (into {}))))

(defn- init-analyzers! [inputs recordable-outputs]
  (let [inputs* (merge-analyzers-inputs inputs recordable-outputs)]
    (doseq [[_ {:keys [analyzer]}] @analyzers-registry]
      (when (and analyzer (o/node-active? analyzer))
        (try (o/kill analyzer)
             (catch Exception _
               (timbre/warn "Signal analyzer does not exist anymore")))))

    (reset! analyzers-registry
            (into {}
                  (map
                    (fn [[input-name {:keys [bus]}]]
                      {input-name (habitat.rec/start-signal-analyzer :input-bus bus)})
                    inputs*)))))

(defn stop!
  [{:keys [reset-bufs?]
    :or {reset-bufs? true}}]
  (gp/stop)
  (ndef/stop)
  (if reset-bufs?
    (reset! habitat.rec/bufs {})
    (when-not (zero? (count @habitat.rec/bufs))
      (timbre/warn "Buffs from the previous session are being preserved")))
  (reset! habitat.rec/recording? {}))



(defn init!
  []
  (let [{:keys [inputs outputs]
         :as init-data}
        (ge.init/init!
          {:inputs-config {:in-1 {:amp (o/db->amp 8)}}
           :outputs-config {:rain-1 {:bh-out 2}
                            :ndef-1 {:bh-out 4}
                            ;; TODO maybe use another `:bh-out`
                            :magma-rain {:bh-out 2}
                            :magma-ndef {:bh-out 4}
                            :estratos-rain {:bh-out 2}
                            :clean-ndef {:bh-out 4}
                            :mantle-plume-rev {:bh-out 4}
                            :mantle-plume-main {:bh-out 6}
                            :erupcion-rain {:bh-out 8}
                            :erupcion-ndef {:bh-out 10}
                            :totalidad-ecosistema {:bh-out 12}
                            :with-rev-send {:bh-out 14}
                            }
           :controls-config {:exp/pedal-1 {:chans 1}
                             :exp/btn-a {:chans 1}
                             :exp/btn-b {:chans 1}
                             :exp/btn-c {:chans 1}
                             :exp/btn-d {:chans 1}
                             :exp/btn-1 {:chans 1}
                             :exp/btn-2 {:chans 1}
                             :exp/btn-3 {:chans 1}
                             :exp/btn-4 {:chans 1}
                             :exp/btn-5 {:chans 1}}})
        ;; Amp analyzer
        amp-analyzers (init-analyzers! inputs outputs)]
    (reaper/init)
    (two.ls/init-watch!)
    (two.interface/init!)
    (assoc init-data :amp-analyzers amp-analyzers)))

(defn init!* [] (println (init!)) nil)

(comment
  (reaper/play)
  (o/stop)

  #_(do (stop! {:reset-bufs? true}) (aseq/stop))
  (aseq/stop)
  (let [sections (concat
                   fondo-oceanico/sections
                   formacion-terrestre/sections
                   erupcion/sections
                   totalidad/sections)]
    (stop! {:reset-bufs? false})
    (init!)
    (aseq/run-sections
      (merge aseq/reaper-events
             {:sections sections
              :start-at 0
              :initial-countdown-seconds 40
              :on-sequencer-end (fn [] (stop! {:reset-bufs? false}))})))


  (erupcion/init-section-buses&outs!)
  (two.ls/set-section (-> erupcion/sections (nth 4)))
  (swap! two.ls/live-state assoc :piece/running? false)
  (aseq/pause)
  (aseq/resume)
  (do (stop! {}) (aseq/stop))
  (aseq/skip)
  (aseq/prev))

(comment
  (require '[tieminos.compositions.garden-earth.moments.two.habitat-in-volcanic-temporality :as ivt])
  (stop! {})
  (o/stop)
  (def init-data (init!))
  (init!*)
  (-> init-data)
  (-> init-data
      :outputs
      deref
      :rain-1
      :synth
      (o/node-active?)
      )

  ;; TODO make this an init function
  ;; For some reason amp via o/sound-in is coming 8db lower than it should be
  ;; so allowing here for compensation.
  ;; FIXME find the cause for the above.
  (ge.init/init!
    {:inputs-config {:in-1 {:amp (o/db->amp 8)}}}))

(comment
  (oe/defsynth io
    ;; Simple input->output synth for testing
    [in 0 out 0]
    (o/out out (* (o/db->amp 8) (o/in in))))

  (def testy (io {:in (fl-i1 :bus)
                 :out (bh 2)}))
  (o/kill testy))

(comment
  (-> @habitat.rec/bufs
      vals
      first
      (->> (into {})))
  (o/demo
   (o/play-buf 1
               (-> @habitat.rec/bufs
                   vals
                   rand-nth)))

  (defn rand-buf []
    (-> @habitat.rec/bufs
        vals
        rand-nth))

  (ref-rain
   :id ::movimientos-subterraneos
   :durs (fn [_] (+ 0.1 (rand 5)))
   :on-event (on-event
                ;; TODO remove use of rand-buf
              (when-let [buf (rand-buf)]
                (println (into {} buf))
                (buf-mvts-subterraneos {:buf buf
                                        :rate (rand-nth [1 11/9 1/2  2/3 4/11
                                                         9/22])
                                        :amp 4
                                        :pan (rrange -1.0 1)
                                        :out (bh 2)}))))
  (gp/stop ::movimientos-subterraneos)
  (gp/stop :ge.two/default-rec-loop)
  (reset! habitat.rec/bufs {})
  (reset! habitat.rec/recording? {})
  (-> habitat.rec/bufs)
  (let [dur 10]
    (start-rec-loop!
     {:input-bus (fl-i1 :bus)
      :rec-dur-fn (fn [_] dur)
      :rec-pulse [dur]}))

  (ndef/ndef
   ::movimientos-subterraneos
   (-> (o/in (fl-i1 :bus))
       (o/lpf 6000) ;; FIXME high pitched noise due to guitar amp line out
       (o/pitch-shift 0.2 [1/2 1/2])
       (o/mix)
       (o/pan2 (lfo-kr 0.3 -1 1))
       (* (o/db->amp 18)
          (lfo-kr 0.5 0.5 1)))
   {:out (bh 4)})

  (ndef/stop ::movimientos-subterraneos)

;;;;;;;;;;

  (ndef/ndef
   ::movimientos-submarinos
   (-> (o/in (fl-i1 :bus))
       (o/lpf 8000) ;; FIXME high pitched noise due to guitar amp line out
       ((fn [sig]
          (o/mix (map (fn [ratio]
                        (-> sig
                            (o/pitch-shift 0.1 ratio)
                            (o/pan2 (lfo-kr 0.5 -1 1))
                            (* (lfo-kr 1 0 1))))

                      [1/2 1 1/4 11/8 2]))))
       (o/free-verb 0.7 2)

       (* (o/db->amp 30)
          (lfo-kr 0.5 0.5 1)))
   {:out (bh 4)})

  (ndef/stop ::movimientos-submarinos)

;;;;;;;;;;;;;;;;
;;; Magma
;;;;;;;;;;;;;;;;

  (oe/defsynth magma
    [buf 0
     rate 1
     dur 1
     a-level 3
     d-level 0.1
     amp 0.5
     start 0
     end 1
     pan 0
     out 0]
    (o/out out
           (-> (o/grain-buf
                :num-channels 1
                :trigger (o/impulse 40)
                :dur dur
                :sndbuf buf
                :rate rate
                :pos (/ (o/phasor:ar 0
                                     (* (/ 1 dur) (o/buf-rate-scale:kr buf))
                                     (* start (- (o/buf-samples:kr buf) 1))
                                     (* end (- (o/buf-samples:kr buf) 1)))
                        (o/buf-samples:kr buf))
                :interp 2
                :pan pan)
               (o/lpf 800) ;; FIXME high pitched noise due to guitar amp line out
               (* 4 (o/env-gen
                     (o/envelope
                        ;; TODO ADSR
                      [0 a-level d-level 0]
                      [(* 0.01 dur)
                       (* 0.79 dur)
                       (* 0.2 dur)])
                     :action o/FREE))
               ;; NOTE rangos (asumiendo a-level 3):
               ;; 0.05 - burbujeos
               ;; 0.3 - magma viva
               ;; 0.5 - ya bastante suave, y brilloso
               ;; 1 - magma brillosa
               ;; Aumentar el a-level aumenta el burbujeo
               (o/sine-shaper 0.5)
               (* 1/2 amp)
               (o/pan2 pan))))

  (ref-rain
   :id ::magma
   :durs #_(fn [_] 1) [1/5]
   :on-event (on-event
                ;; TODO remove use of rand-buf
                ;; Por ahora usando samples de atractores/drone guitarra
              (when-let [buf (-> @habitat.rec/bufs vals rand-nth)]
                (println "Dur" (* 1 (:duration buf)))
                (magma {:buf buf
                        :rate (at-i [1 3/2 1/2
                                     #_7/4
                                     #_11/4 ;; buena distor
                                     ])
                        :dur  (weighted {1 5
                                         2 1
                                         3 4
                                         9 10})
                        :start (rand)
                        :end (rand)
                          ;;  Incluso llegando a 10 suena muy chingon y la secuencia funciona bien
                        :a-level (at-i [3
                                          ;; 2 3 4 5
                                          ;; 6 7 8 9 10
                                        ])
                        :d-level 0.1
                        :amp 1/2
                        :pan (rrange -1.0 1)
                        :out (bh 2)}))))

  (gp/stop ::magma)

  (ref-rain
   :id ::sucesion-especies
    ;; TODO control durs so that this could go faster or slower
   :durs (fn [_] (+ 0.1 (rand 2)))
   :on-event (on-event
              (let [buf (-> @habitat.rec/bufs vals rand-nth)
                    scale (at-i [#_"1)3 of 3)6 1.5-7.9.11"
                                 #_"2)4 of 3)6 11-1.3.7.9"
                                 "2)4 of 3)6 1-3.5.7.9"])
                    arp-config {;; TODO improve control of scale
                                :scale (subcps scale)
                                :out (bh 4)
                                  ;; TODO use custom interval-seq-fn
                                :interval-seq-fn (comp (partial map #(+ (rand-int -10) %))
                                                       default-interval-seq-fn)
                                  ;; envs can grow from [1, 3] to [8, 17] - maybe weight in different ways
                                :env-min-dur 8
                                :env-max-dur 17
                                :amp-min 0.3
                                :amp-max 1}
                    arp-data {:pitch-class "A+92"
                                ;; TODO use bufs from another section
                                ;; TODO allow custom synth
                                ;; TODO allow general control of amp - uses bezier amp curve so general-amp*bezier-curve
                              :buf buf}]
                (when (> 0.5 (rand))
                    ;; Bass
                  (arp-reponse-2 (assoc arp-config
                                        :interval-seq-fn (comp (partial map #(+ (rand-int -40) %))
                                                               default-interval-seq-fn))
                                 arp-data)
                    ;; TODO make a good synth for this section that will play the
                    ;; current buf in a nice way... depends on what is going to be played
                    ;; but perhaps some transparentish granulation or a longish play-buf
                    ;; could be nice...
                    ;; Perhaps even play this before the other synths or something, e.g. delayed.
                  #_(movimientos-subterraneos {:buf buf
                                               :rate (rand-nth [1/4])
                                               :amp 3
                                               :pan (rrange -1.0 1)
                                               :out (bh 2)}))
                  ;; Highs
                (arp-reponse-2 arp-config arp-data))))
  (gp/stop ::sucesion-especies)

  (-> eik :subcps keys)

;;;;;;;;;;;;;;;;
;;; PS TODO finish this... wrap up into some specific thing(s)
;;;;;;;;;;;;;;;;

  (oe/defsynth test-delayed-pitch-shifter
    [ratio 1
     out 0]
    (o/out out
           (let [sig (-> (o/saw (* ratio 400))
                         (* 0.5 (o/env-gen (o/env-perc 1 1)
                                           :action o/FREE)))]
             (+ sig))))

  (def test-bus (o/audio-bus 1 "test-bus"))

  (ndef/ndef
   ::simple-pitch-shifter
   (let [sig (o/in test-bus)]
     (+ #_sig
      (-> sig
          (o/pitch-shift 0.05 2)
          (o/delay-n:ar 1 1))))
   {:out (bh 4)
    :fade-time 2})

  (oe/defsynth
    simple-pitch-shifter
    [in 0
     dur 2
     rate 1
     delay 0
     pan 0
     amp 1
     out 0]
    (let [sig (o/in in)]
      (o/out out
             (+
               ;; TODO remove pure sig, just for testing
              (* 0.05 sig)
              (-> sig
                  (o/pitch-shift:ar 0.05 rate)
                  (o/delay-n:ar delay delay)
                  (o/free-verb)
                  (* amp (o/env-gen (o/envelope [0 0 1 1 0]
                                                [delay
                                                 (* 0.2 dur)
                                                 (* 0.6 dur)
                                                 (* 0.2 dur)])
                                    :action o/FREE))
                  (o/pan2 pan))))))

  ;; NOTE this seems useful for strata and mountain texture
  (let [in test-bus]
    (ref-rain
     :id :ps/simple
     :durs [1]
     :on-event (on-event
                (simple-pitch-shifter
                 {:group (groups/mid)
                  :in in
                  :dur 4
                  :rate (rand-nth [1 #_#_#_#_#_3/2 1/2 3 5/2 7/2 11/2 13/2])
                  :delay (rand 2)
                  :pan (rrange -1 1)}))))

  (ref-rain
   :id :test/pitch-shifter
   :durs [1]
   :on-event (on-event
              (test-delayed-pitch-shifter {:group (groups/early)
                                           :out test-bus
                                           :ratio (at-i [1 9/8])})))

  (o/stop)

  (gp/stop)

;;;;;;;;;;;;;;;;
;;; Amplitude reactivity ... wrap up into some specific thing(s)
;;;;;;;;;;;;;;;;

  (def t2 (amp-trig/reg-amp-trigger
           {:in (fl-i1 :bus)
            :thresh (o/db->amp -21)
            :amp-trigger-params {:group (groups/mid)
                                  ;; useful for percussive triggers
                                 :lag 0 :amp-rel-time 0.01}
            :handler (fn [args]
                       (println (rand-int 10000) args)
                       (synths/low :freq (rrange 200 900)))
            :args {:a :b}}))

  (amp-trig/dereg-handler t2))

(comment
  ;; Rec loop
  (-> @habitat.rec/bufs)
  (reset! habitat.rec/bufs {})
  (let [dur 2]
    (start-rec-loop!
     {:input-bus (fl-i1 :bus)
      :section "fondo-oceanico"
      :subsection "fondo-oceanico"
      :rec-dur-fn (fn [_] dur)
      :rec-pulse [dur]}))
  (gp/stop :ge.two/default-rec-loop))
