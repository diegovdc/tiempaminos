(ns tieminos.piraran.with-rainbow-trash.core
  (:require
   [hacia-un-nuevo-universo.nebulosa&planeta-hostil :as-alias nebulosa&planeta-hostil]
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.ui.v1 :refer [add-rec-bar]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [rec-input recording?]]
   [tieminos.habitat.routing :refer [inputs main-returns preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [quad-router-2o rev-filter
                                                 start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(do
  (def rates (->> (for [h (range 1 14)
                        sub (range 1 6)]
                    (/ h sub))
                  shuffle
                  (take 3)))
  rates)

(defn rand-latest-buf []
  (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 3) (rand-nth)))

(oe/defsynth bow
  [buf 0]
  (o/out 0 (+ (* 0.2 (o/env-gen (o/env-perc) :action o/FREE) (o/sin-osc 200))
              (o/play-buf 1 buf))))

(oe/defsynth bow
  [buf 0
   trig-rate 40
   grain-dur 1/20
   rate 1
   amp 1
   amp-lfo-min 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   d 1
   d-level 0.3
   r 3
   out 0
   lpf-min 100
   lpf-max 20000
   pan 0
   rev-mix 1
   rev-room 0.5
   a-level 1
   delay 0]
  (o/out out
         (-> (o/grain-buf
              :num-channels 1
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos  (o/line start end (+ a d r))
              :interp 4
              :pan 0)
             (o/delay-n delay)
             (o/rlpf (lfo 4 lpf-min lpf-max) 0.2)
             (#(+ % (* 2 (o/b-moog % (lfo 0.4 500 2000) (lfo 2 0.2 0.6) 2))))
             (#(o/pan-az 2 % :pos pan :width (lfo 0.4 1 2.5)))
             (o/free-verb rev-mix rev-room)
             (* amp
                (lfo amp-lfo amp-lfo-min 1)
                (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                       [-1 -5])
                           :action o/FREE)))))

(defn rec [bus dur-s]
  (rec-input {:section "rt"
              :subsection "1"
              :input-name (:name bus)
              :input-bus bus
              :dur-s dur-s
              :on-end (fn [_])
              :on-rec-start add-rec-bar}))

(defn start-rec-loop!
  [bus durs]
  (ref-rain
   :id :rec-loop
   :durs durs
   :on-event (on-event (rec bus (max 0.01 (- dur-s 0.1))))))

(oe/defsynth rev [in 0 out 0 mix 0.5 room 1]
  (o/out out (-> in
                 (o/in 2)
                 (o/free-verb mix room))))

(comment
  (groups/init-groups!)
  (o/stop)
  (ndef/ndef ::input-test
             (o/pan2 (o/sound-in 0)))
  (ndef/stop ::input-test)
  (start-rec-loop! 0 [3])
  (reset! recording? {})
  (reset! rec/bufs {})
  (gp/stop :rec-loop)
  (gp/stop ::rainbow-trash3)

  (def rev-bus (o/audio-bus 2 "rev-bus"))
  (rev {:group (groups/fx)
        :in rev-bus})
  (-> @rec/bufs vals rand-nth)
  (ref-rain
   :id ::rainbow-trash
   :durs [1 1 1/2 1 1/2 1 1/2]
   :ratio 1/8
   :on-event (on-event
              (let [buf (-> @rec/bufs vals rand-nth)
                    x (* 0.3 (rand))
                    t (inc (rand-int 40))
                    params {:group (groups/early)
                            :trig-rate t
                            :grain-dur (/ t 2)
                            :buf buf
                            :amp (at-i [3 2 7 3 2 1 7])
                            :rate (at-i [3 2 1 (at-i [4 3 5/4 5])])
                            :a 0.5
                            :d 1/16
                            :d-level 0.4
                            :start (+ x 0.001)
                            :end (+ x 0.2)
                            :rev-room (rrange 0.3 1)
                            :rev-mix (rrange 0.5 1)
                            :r 1/4
                            :pan (rrange -1 1)
                            :out rev-bus}]
                (bow params)
                #_(bow (-> params

                         (assoc
                           :amp 1
                           :a (rrange 0.1 1)
                           :rate 3/2
                           :pan (rrange -1 1)
                           :delay 3/7)))
                #_(bow (-> params
                         (update :amp * 0.5)
                         (update :r * 2)
                         (assoc :d (rrange 0.01 2)
                                :rate (-> 6/5
                                          (/ (rand-nth [1 2 3 4 5]))
                                          (* (rand-nth [1 2 3 4 5])))
                                :pan (rrange -1 1)
                                :delay 5/7))))))
  (ref-rain
    :id ::rainbow-trash3
   :ref ::rainbow-trash
    :durs [1]
    :ratio 1/7
    :on-event (on-event
                (let [buf (-> @rec/bufs vals rand-nth)
                      x (* 0.3 (rand))
                      params {:group (groups/early)
                              :trig-rate 3
                              :grain-dur 1/40
                              :buf buf
                              :amp (* (rrange 0.1 0.5) (at-i [3 2 7 3 2 1 7]))
                              :rate (* 5 (at-i [3 2 1 (at-i [4 3 5/4 5])]))
                              :a 2
                              :d 1/64
                              :d-level (rand)
                              :start (+ x 0.001)
                              :end (+ x 0.002)
                              :rev-room 3
                              :rev-mix (rrange 0.5 1)
                              :r 1/64
                              :pan (rrange -1 1)
                              :out rev-bus}]
                  (bow params)
                  #_(bow (-> params
                           (update :amp * 0.1)
                           (update :rate #(-> %
                                            (/ (rand-nth [1 2 3 4 5]))
                                            (* (rand-nth [1 2 3 4 5]))))
                           (assoc :d (rrange 0.01 2)

                                  :pan (rrange -1 1)
                                  :delay 5/7))))))
  (ref-rain
   :id ::rainbow-trash2
   :ref ::rainbow-trash
   :ratio 1/3
   :on-event (on-event
              (let [buf (-> @rec/bufs vals last)
                    params {:group (groups/early)
                            :trig-rate 80
                            :grain-dur 1/10
                            :buf buf
                            :amp (* 2 (at-i [3 2 1]))
                            :rate (/ 1 (at-i [3 2 1 4]))
                            :a 0.2
                            :d 1/16
                            :d-level 0.4
                            :start 0.001
                            :end 0.002
                            :rev-room 0.2
                            :r 1/4
                            :pan (rrange -1 1)
                            :out rev-bus}]
                (bow params)
                #_(bow (assoc params
                              :amp 1
                                ;; :a (rrange 0.1 1)
                              :rate 3/2
                              :pan (rrange -1 1)
                              :delay 3/7))
                #_(bow (assoc params
                                ;; :d (rrange 0.01 2)
                              :rate (/ 5/3 (rand-nth [1 2 3 4 5]))
                              :pan (rrange -1 1)
                              :delay 5/7))))))

(comment

  (gp/stop)
  (o/stop)

  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! recording? {})
    (reset! rec/bufs {}))
  (do)
  (init!)
  (def section-out (o/audio-bus 4 "section-out-bus"))
  (def qbr-out1 (o/audio-bus 4 "qbr-out-bus"))

  (quad-router-2o
   {:group (groups/mid :tail)
    :in-bus section-out
    :out-bus1 qbr-out1
    :out-bus2 (main-returns :mixed)})

  (rev-filter
   {:group (groups/panners)
    :in-bus qbr-out1})

  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts})

  (start-rec-loop3!
   {:input-bus-fn (fn [_] (-> @inputs (select-keys [#_:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
    :durs (mapv (fn [_] (rrange 10 20)) (range 40))})

  (rainbow-trash
   {::nebulosa&planeta-hostil/out-bus section-out})
  :rcf)
