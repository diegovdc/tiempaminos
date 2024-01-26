(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trig
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [tieminos.habitat.amp-trigger :as amp-trig :refer [reg-amp-trigger]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [silence?]]
   [tieminos.habitat.routing :refer [inputs mixed-main-out
                                     percussion-processes-main-out preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [amanecer*guitar-clouds-2
                                                 start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.math.utils :refer [hyperbolic-decay linearly-weighted-avg]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :as scu]
   [tieminos.utils :refer [rrange]]
   [time-time.standard :refer [rrand]]))

(defn get-rand-buf []
  (->> @rec/bufs
       vals
       (sort-by :rec/time)
       reverse
       (filter :analysis)
       (remove #(silence? 0.01 %))
       (take 2)
       (#(when (seq %) (rand-nth %)))))

(defn play-sample [{:keys [out]}]
  (let [buf (get-rand-buf)
        trig-rate (weighted {40 2 100 5 80 2})
        start (rrand 0 0.9)
        end (rrand (+ start 0.01) 1)]
    (when buf
      (amanecer*guitar-clouds-2 {:group (groups/mid)
                                 :buf buf
                                 :a (weighted {3 3
                                               5 1
                                               0.2 1})
                                 :d (weighted {3 3
                                               5 1
                                               0.1 1})
                                 :r (weighted {3 3
                                               5 1
                                               2 1
                                               1 0.5})
                                 :rate (weighted {1/2  2
                                                  1    5
                                                  3/2  5
                                                  2    3
                                                  11/4 2
                                                  7/4  3})
                                 :d-level (weighted {0.7 2 0.5 5 0.1 2})
                                 :rev-room (rrand 2 12)
                                 :trig-rate trig-rate
                                 :grain-dur (/ 1 (/ trig-rate 2))
                                 :amp-lfo (rrange 0.1 0.4)
                                 :amp-lfo-min 0.95
                                 :amp (weighted {1 1 0.5 5 0.7 3 0.8 2 0.9 1})
                                 :lpf-min 300
                                 :lpf-max (rrange 2000 10000)
                                 :start start
                                 :end end
                                 :out out
                                 :pan (rrange -1 1)}))))

(comment
  (when @habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {})
    (reset! amp-trig/handlers {}))
  (init!)

  (comment
    ;; Amplitudes
    {:guitar {:scarlett 4.5 :ampli [5 7 8] :amp-trig/threshold :default}
     :mic-1 {:scarlett 6 :amp-trig/threshold :default}
     :mic-2 {:scarlett 7 :amp-trig/threshold :default}
     :mic-3/lavalier {:scarlett 8 :amp-trig/threshold :default}
     :scarlett/out 8})

  (def ps-ringz-amp (atom 1))

  (do
    (defn set-ps-ringz-amp!
      [peak-amp]
      (reset! ps-ringz-amp (hyperbolic-decay peak-amp 2 0.7)))
    (set-ps-ringz-amp! 0)

    (defn init-amp-regulator-receiver!
      []
      (let [last-peak-amps (atom ())]
        (o/on-event "/amp-regulator"
                    (fn [data]
                      (let [peak-amp (-> data :args (nth 2))
                            last-peaks (swap! last-peak-amps (comp #(take 10 %) conj) peak-amp)
                            peak-avg (linearly-weighted-avg last-peaks)]
                        (set-ps-ringz-amp! peak-avg)))
                    :amp-regulator-handler)))

    (oe/defsynth amp-regulator-replier
      [in 0
       freq 2
       reply-id 1]
      (let [impulse (o/impulse freq)]
        (o/send-reply  impulse
                       "/amp-regulator"
                       [(o/amplitude:kr (apply max (o/peak:ar
                                                    (o/in:ar in 4)
                                                    (o/delay2:kr impulse))))]
                       reply-id)))

    ;; NOTE keep, only ps below
    (oe/defsynth ps-ringz
      [in 0
       ps1 1
       ps2 1
       rz-freq 200
       amp 1
       out 0]
      (o/out out (let [sig (o/mix [(o/in (-> @inputs :mic-1 :bus) 1)
                                   (o/in (-> @inputs :mic-2 :bus) 1)
                                   (o/in (-> @inputs :mic-3 :bus) 1)
                                   (* 1.5  (o/in (-> @inputs :guitar :bus) 1))])
                       sig2 (+ #_(-> sig
                                     (o/pitch-shift 0.1 ps1)
                                     (* 0.8)
                                     (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                             (-> sig
                                 (o/pitch-shift 0.1 ps2)
                                 #_(* 0.8)
                                 (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                               (-> sig
                                   (o/ringz rz-freq 0.1)
                                   (* 0.07)
                                   (o/limiter 0.9 0.005)
                                   #_(o/lpf 8000)
                                   (#(o/pan-az 4 % (scu/lfo-kr (scu/lfo-kr 2 1 5) -1 1)))))]
                   (-> (+ sig2
                          #_(-> sig2
                                (o/comb-l 0.2 (scu/lfo-kr 1 0.15 0.2) 1)
                                (o/pitch-shift 0.1 ps2 0 0.0001)
                                (* 0.5 (o/env-gen (o/env-perc 2 1  0.2 0.7)))))
                       (o/limiter 0.9 0.005)
                       (o/free-verb (scu/lfo-kr 2 0.4 0.7) 2 0)
                       (* amp (o/env-gen (o/envelope [0 1 1 0.65 0.6 0]
                                                     [0.3 0.5 1.5 4 2])
                                         :time-scale 1
                                         :action o/FREE))))))

    (oe/defsynth filtered-rev-long-tail
      []
      (o/out 0 (o/silent)))

    (defn mic-1-amp-trig-handler
      [{:keys [in]}]
      (println :trig/mic-1 (java.util.Date.))
      (ps-ringz {:group (groups/mid)
                 :in in
                 :ps1 3/2
                 :ps2 (* (rand-nth [1 2]) (rand-nth [1/4 11/32 4/11 13/32]))
                 :rz-freq (* (rand-nth [200 250 300])
                             (rand-nth [1 2 3 4 5]))
                 :amp @ps-ringz-amp
                 :out percussion-processes-main-out}))

    (defn mic-2-amp-trig-handler
      [{:keys [in]}]
      (println :trig/mic-2 (java.util.Date.))
      (ps-ringz {:group (groups/mid)
                 :in in
                 :ps1 3/2
                 :ps2 (* (rand-nth [1 2]) (rand-nth [1/4 11/32 4/11 13/32]))
                 :rz-freq (* (rand-nth [200 250 300])
                             (rand-nth [1 2 3 4 5]))
                 :amp @ps-ringz-amp
                 :out percussion-processes-main-out}))

    (defn mic-3-amp-trig-handler
      [{:keys [_in]}]
      (println :trig/mic-3 (java.util.Date.))
      (play-sample {:out mixed-main-out}))

    (defn guitar-amp-trig-handler
      [{:keys [_in]}]
      (println :trig/guitar (java.util.Date.))
      (play-sample {:out mixed-main-out})))

  (start-rec-loop3!
   {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2 :mic-3]) vals (->> (map :bus))))
    :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
  (do
    (def mic-1-ampt (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                                      :handler #'mic-1-amp-trig-handler}))
    (def mic-2-ampt (reg-amp-trigger {:in (-> @inputs :mic-2 :bus)
                                      :handler #'mic-2-amp-trig-handler}))
    (def mic-3-ampt (reg-amp-trigger {:in (-> @inputs :mic-3 :bus)
                                      :handler #'mic-3-amp-trig-handler}))
    (def guitar-ampt (reg-amp-trigger {:in (-> @inputs :guitar :bus)
                                       :handler #'guitar-amp-trig-handler})))

  (do
    (init-amp-regulator-receiver!)

    (def ar (amp-regulator-replier
             {:group (groups/fx)
              :in percussion-processes-main-out})))

  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts})

  (do
    (amp-trig/dereg-handler mic-1-ampt)
    (amp-trig/dereg-handler mic-2-ampt)
    (amp-trig/dereg-handler mic-3-ampt)
    (amp-trig/dereg-handler guitar-ampt)))
