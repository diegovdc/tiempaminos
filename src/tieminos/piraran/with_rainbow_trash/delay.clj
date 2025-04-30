(ns tieminos.piraran.with-rainbow-trash.delay
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [guitar-processes-main-out inputs
                                     main-returns percussion-processes-main-out
                                     preouts]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.synths :refer [soft-saw2]]
   [tieminos.utils :refer [ctl-synth2 wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(oe/defsynth bp-delay
  [in 0
   out 0
   delay-time 1/5
   bp-rate 1
   bp-rq-min 0.05
   bp-rq-max 1
   room 1
   ratio 3/2
   amp 1
   gate 1]
  (let [shifted (o/pitch-shift (o/sound-in in 1) 0.1
                               ratio
                               0
                               0)
        delayed (-> (+ (o/sound-in in 1) (* (lfo 0.1 0.5 1) shifted))
                    (o/comb-n 1 delay-time 10)
                    o/leak-dc
                    (o/bpf #_(lfo0-kr (* 3 delay-time) 200 3000)
                     (o/lin-lin:kr (o/lf-noise2:kr bp-rate) -1 1 200 3000) ;; perhaps use lag or lagud instead of lf-noise
                           (lfo 0.1 bp-rq-min bp-rq-max))
                    (o/free-verb 0.7 room 0)
                    (o/pan2 (lfo 1 -1 1))
                    (* amp (lfo 0.1 1 3)
                       (o/env-gen (o/env-adsr 3 1 1 5)
                                  :gate gate :action o/FREE)))]
    (o/out out delayed)))

(defn make-bp-delay-bank
  [{:keys [group in out configs]}]
  (let [base-config {:group group
                     :in 2
                     :bp-rate 1
                     :delay-time 1/2
                     :amp 0.8
                     :room 5
                     :out out}
        delays (mapv (fn [config]
                       (bp-delay (merge base-config config)))
                     configs)
        stop! (fn [] (doseq [synth delays]
                       (o/ctl synth :gate 0)))]
    {:delays delays :stop! stop!}))

(map-indexed vector [1 2 3])

(defn make-refrains
  [id-base-str
   delays
   refrain-configs]
  (doseq [[i synth] (map-indexed vector delays)]
    (let [{:keys [durs make-on-event]} (wrap-at i refrain-configs)]
      (ref-rain :id (keyword id-base-str (str "delay-" i))
                :durs durs
                :on-event (make-on-event synth)))))

(comment
  (groups/init-groups!)
  (def delay-in (o/audio-bus 1 "delay-in"))
  (def delay-in2 (o/audio-bus 1 "delay-in2"))
  (def delay-out (o/audio-bus 1 "delay-out"))

  (o/stop)

  (oe/defsynth pan-out
    [in 0]
    (o/out 0 (o/pan2 (o/in in 1) 0)))

  (do (doseq [synth [delay1 delay2 delay3]]
        (o/ctl synth :gate 0))
      (doseq [id [:ratito :ratito3 :ratito2]]
        (gp/stop id)))

  (def delay-bank1 (make-bp-delay-bank
                    {:group (groups/mid)
                     :out 1
                     :configs [{:delay-time 1/2}
                               {:delay-time 1}
                               {:delay-time 3}]}))

  (o/free delay-bank1)

  (make-refrains
   "bank1" (:delays delay-bank1)
   [{:durs [3]
     :make-on-event #(on-event (ctl-synth2 % :ratio (at-i [1 2 4 8])
                                           :amp 2))}
    {:durs [3/8]
     :make-on-event #(on-event (ctl-synth2 % :ratio (at-i (map (fn [i] (* 2 i)) (range 13 26)))
                                           :amp 0))}
    {:durs [1]
     :make-on-event #(on-event (ctl-synth2 % :ratio (at-i (map (fn [i] (/ 1 i)) (range 13 24)))
                                           :amp 0))}])
  (make-refrains
   "bank2" (:delays delay-bank2)
   [{:durs [3]
     :make-on-event #(on-event (ctl-synth2 % :ratio (at-i [1 2 7/4 6/5])))}
    {:durs [3/8]
     :make-on-event #(on-event (ctl-synth2 % :ratio (/ (* 2 (inc (rand-int 12)))
                                                       (inc (rand-int 12)))))}
    {:durs [1]
     :make-on-event #(on-event (ctl-synth2 % :ratio (/ (* 2 (inc (rand-int 12)))
                                                       (inc (rand-int 12)))))}])

  (doseq [synth (:delays delay-bank1)]
    (o/ctl synth :room 3 :amp 0.6)) ;; TODO agregar control continuo
  (doseq [synth (:delays delay-bank2)]
    (o/ctl synth :room 2 :amp 0.3))

  (let [config {:group (groups/mid)
                :in (-> @inputs :mic-1 :bus)
                :bp-rate 1
                :delay-time 1/2
                :amp 0.8
                :room 5
                :out (main-returns :mixed)}]
    (def delay1 (bp-delay (assoc config :delay-time 1/2)))
    (def delay2 (bp-delay (assoc config :delay-time 1)))
    (def delay3 (bp-delay (assoc config :delay-time 3))))

  (gp/stop)
  (do
    (ref-rain :id :ratito
              :durs [3]
              :on-event (on-event
                         "hola"))
    (ref-rain :id :ratito2
              :ref :ratito
              :ratio 1/8
              :on-event (on-event
                         (ctl-synth2 delay1 :ratio (/ (* 2 (inc (rand-int 12)))
                                                      (inc (rand-int 12))))))
    (ref-rain :id :ratito3
              :ref :ratito
              :ratio 1/3
              :on-event (on-event
                         (ctl-synth2 delay3 :ratio (/ (inc (rand-int 12))
                                                      (inc (rand-int 12)))))))

  (def oxygen (get-oxygen!))
  (midi-in-event
   :midi-input oxygen
   :note-on (fn [{:keys [note velocity]}]
              (soft-saw2 (groups/early)
                         :freq (midi->cps note)
                         :amp (linexp* 0 127 0.5 1 velocity)
                         :out delay-in2)))

  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! rec/recording? {})
    (reset! rec/bufs {}))
  (init!)
  (-> percussion-processes-main-out)
  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts}))
