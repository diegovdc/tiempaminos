(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trig
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.amp-trigger :as amp-trig :refer [reg-amp-trigger]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [silence?]]
   [tieminos.habitat.routing :refer [guitar-processes-main-out inputs
                                     mixed-main-out percussion-processes-main-out
                                     preouts]]
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
  ;; Amplitudes
  {:guitar {:scarlett 4.5 :ampli [5 7 8] :amp-trig/threshold :default}
   :mic-1 {:scarlett 6 :amp-trig/threshold :default}
   :mic-2 {:scarlett 7 :amp-trig/threshold :default}
   :mic-3/lavalier {:scarlett 7 :amp-trig/threshold :default}
   :scarlett/out 8})

(def ps-ringz-amp (atom 1))

(defn add-ringz-group
  [groups]
  (assoc groups ::ringz (o/group :after (:early groups))))

(defn ringz-group
  ([] (ringz-group :tail))
  ([pos] [pos (::ringz @groups/groups)]))

(defn set-ps-ringz-amp!
  [peak-amp]
  (let [amp (hyperbolic-decay peak-amp 2 0.7)]
    (when (not= amp @ps-ringz-amp)
      (reset! ps-ringz-amp amp)
      (o/ctl (::ringz @groups/groups) :amp @ps-ringz-amp)
      (timbre/info :ps-ringz-amp amp))))

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

(oe/defsynth ps-ringz-4ins
  [mic-1 0 mic-2 0 mic-3 0 guitar 0 out 0]
  (o/out out (o/mix [(* 1.4 (o/in mic-1 1))
                     (* 2 (o/in mic-2 1))
                     (* 1.6 (o/in mic-3 1)) ;; 0.7 cures feedback
                     (* 0.6 (o/in guitar 1))])))

(oe/defsynth ps-ringz
  [in 0
   ps2 1
   rz-freq 200
   amp 1
   out 0]
  (o/out out (let [sig (o/in in 1)
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
                               #_(#(o/compander % % 0.6 1 1/5))
                               #_(o/limiter 0.7 0.005)
                               #_(o/lpf 8000)
                               (#(o/pan-az 4 % (scu/lfo-kr (scu/lfo-kr 2 1 5) -1 1)))))]
               (-> (+ sig2
                      #_
                      (-> sig2
                          (o/comb-l 0.2 (scu/lfo-kr 1 0.15 0.2) 1)
                          (o/pitch-shift 0.1 ps2 0 0.0001)
                          (* 0.5 (o/env-gen (o/env-perc 2 1  0.2 0.7)))))
                   (o/limiter 0.9 0.005)
                   (o/free-verb (scu/lfo-kr 2 0.4 0.7) 2 0)
                   (* (o/lag amp 0.5) (o/env-gen (o/envelope [0 1 1 0.65 0.6 0]
                                                             [0.3 0.5 1.5 4 2])
                                                 :time-scale 1
                                                 :action o/FREE))))))

(defn mic-1-amp-trig-handler
  [{:keys [args]}]
  (timbre/info :trig/mic-1)
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (* (rand-nth [200 250 300])
                         (rand-nth [1 2 3 4 #_5]))
             :amp @ps-ringz-amp
             :out percussion-processes-main-out}))

(defn mic-2-amp-trig-handler
  [{:keys [args]}]
  (timbre/info :trig/mic-2)
  (ps-ringz {(ringz-group) :group
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (* (rand-nth [200 250 300])
                         (rand-nth [1 2 3 4 #_5]))
             :amp @ps-ringz-amp
             :out percussion-processes-main-out}))

(defn mic-3-amp-trig-handler
  [{:keys [_in args]}]
  (timbre/info :trig/mic-3)
  (play-sample {:group (groups/mid)
                :out mixed-main-out})
  (ps-ringz {(ringz-group) :group
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (* (rand-nth [200 250 300])
                         (rand-nth [1 2 3 4 #_5]))
             :amp @ps-ringz-amp
             :out percussion-processes-main-out}))

(defn guitar-amp-trig-handler
  [{:keys [in args]}]
  (timbre/info :trig/guitar)
  (when (> (rand) 0.4)
    (play-sample {:group (groups/mid)
                  :out mixed-main-out})
    (ps-ringz {:group (ringz-group)
               :in (:in-bus args)
               :ps1 3/2
               :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
               :rz-freq (* (rand-nth [200 250 300])
                           (rand-nth [1 2 3 4 #_5]))
               :amp @ps-ringz-amp
               :out guitar-processes-main-out})))

(comment
  (when @habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-Sequencer! hseq/context)
    (reset! rec/bufs {})
    (reset! amp-trig/handlers {}))
  (init! {:add-custom-groups-fn add-ringz-group})

  ;; NOTE IMPORATANT do not forget to add the `add-ringz-group` `:add-custom-groups-fn` to the `init!` function's config
  (do

    (def ps-ringz-4ins-bus (o/audio-bus 1 ps-ringz-4ins-bus))
    (def ps-ringz-4ins* (ps-ringz-4ins
                         {:group (ringz-group :head)
                          :mic-1 (-> @inputs :mic-1 :bus)
                          :mic-2 (-> @inputs :mic-2 :bus)
                          :mic-3 (-> @inputs :mic-3 :bus)
                          :guitar (-> @inputs :guitar :bus)
                          :out ps-ringz-4ins-bus}))

    (def mic-1-ampt (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                                      :thresh 0.01
                                      :handler #'mic-1-amp-trig-handler
                                      :handler-args {:in-bus ps-ringz-4ins-bus}}))
    (def mic-2-ampt (reg-amp-trigger {:in (-> @inputs :mic-2 :bus)
                                      :thresh 0.005
                                      :handler #'mic-2-amp-trig-handler
                                      :handler-args {:in-bus ps-ringz-4ins-bus}}))
    (def mic-3-ampt (reg-amp-trigger {:in (-> @inputs :mic-3 :bus)
                                      :thresh 0.1
                                      :handler #'mic-3-amp-trig-handler
                                      :handler-args {:in-bus ps-ringz-4ins-bus}}))
    (def guitar-ampt (reg-amp-trigger {:in (-> @inputs :guitar :bus)
                                       :handler #'guitar-amp-trig-handler
                                       :handler-args {:in-bus ps-ringz-4ins-bus}})))

  (o/ctl mic-3-ampt :thresh 0.002)
  (do
    (init-amp-regulator-receiver!)

    (def ar (amp-regulator-replier
             {:group (groups/fx)
              :in percussion-processes-main-out})))

  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts})

  (start-rec-loop3!
   {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2 :mic-3]) vals (->> (map :bus))))
    :durs (mapv (fn [_] (rrange 10 20)) (range 40))
    :rec-input-config {:print-info? false}})

  (do
    (o/kill ps-ringz-4ins*)
    (amp-trig/dereg-handler mic-1-ampt)
    (amp-trig/dereg-handler mic-2-ampt)
    (amp-trig/dereg-handler mic-3-ampt)
    (amp-trig/dereg-handler guitar-ampt)))
