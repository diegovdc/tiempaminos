(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trig
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.amp-trigger :as amp-trig :refer [reg-amp-trigger]]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scsyndefs.core :refer [amp-regulator-replier]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [silence?]]
   [tieminos.habitat.routing :refer [inputs main-returns mixed-main-out
                                     percussion-processes-main-out preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [amanecer*guitar-clouds-2
                                                 start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.math.utils :refer [hyperbolic-decay]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :as scu]
   [tieminos.utils :refer [rrange throttle2]]
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

(defonce ps-ringz-amp-reg-scale (atom 8))
(defonce ps-ringz-amp-reg-thresh (atom (o/db->amp -32)))
(defonce log-amp-peak-db? (atom false))

(comment
  (reset! log-amp-peak-db? true)
  (o/amp->db
   (hyperbolic-decay (o/db->amp -10)
                     10
                     (o/db->amp -30))) ; => -13.706691356885635

  ;; multiplying amp = adding db
  (o/amp->db (* (o/db->amp -10)
                (o/db->amp -13.706691356885635))))

(defn set-ps-ringz-amp!
  [peak-amp]
  (when @log-amp-peak-db?
    (println (o/amp->db peak-amp)))
  (let [amp (hyperbolic-decay peak-amp
                              @ps-ringz-amp-reg-scale
                              @ps-ringz-amp-reg-thresh)]
    (when (not= amp @ps-ringz-amp)
      (reset! ps-ringz-amp amp)
      (o/ctl (::ringz @groups/groups) :amp @ps-ringz-amp)
      (timbre/info :ps-ringz-amp amp))))

(defn init-amp-regulator-receiver!
  []
  (o/on-event "/amp-regulator"
              (fn [data]
                #_(println data)
                (let [peak-amp (->> data :args (drop 2) (apply +))]
                  (set-ps-ringz-amp! peak-amp)))
              :amp-regulator-handler))

(oe/defsynth amp-regulator-ins
  [guitar 0
   percussion 0
   guitar-processes 0
   percussion-processes 0
   mixed 0
   non-recordable 0
   out 0]
  (o/out out (o/mix [(o/in guitar 4)
                     (o/in percussion 4)
                     (o/in guitar-processes 4)
                     (o/in percussion-processes 4)
                     (o/in mixed 4)
                     (o/in non-recordable 4)])))

(oe/defsynth ps-ringz-4ins
  [mic-1 0
   mic-2 0
   mic-3 0
   guitar 0
   mic-1-amp 1.4
   mic-2-amp 2
   mic-3-amp 4
   guitar-amp 0.9
   out 0]
  (o/out out (o/mix [(* mic-1-amp (o/in mic-1 1))
                     (* mic-2-amp (o/in mic-2 1))
                     (* mic-3-amp (o/in mic-3 1))
                     (* guitar-amp (o/in guitar 1))])))

(oe/defsynth ps-ringz*
  [in 0
   ps2 1
   rz-freq 200
   amp 1
   time-scale 1
   e1 1
   e2 0.8
   e3 0.65
   out 0]
  (o/out out (let [sig (o/in in 1)
                   sig2 (+ #_(-> sig
                                 (o/pitch-shift 0.1 ps1)
                                 (* 0.8)
                                 (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                           (-> sig
                               (o/pitch-shift 0.1 ps2)
                               #_(* 0.8))
                           (-> sig
                               (o/pitch-shift 0.1 (* 2/3 ps2))
                               (* 0.8))
                           #_(-> sig
                                 (o/ringz rz-freq 2)
                                 (* 0.07 (scu/lfo-kr 9 0.2 0.7))
                                 #_(#(o/compander % % 0.6 1 1/5))
                                 #_(o/limiter 0.7 0.005)
                                 #_(o/lpf 8000)
                                 (#(o/pan-az 4 % (scu/lfo-kr (scu/lfo-kr 2 1 5) -1 1)))))
                   sig2* (o/pan-az 4 (+ (* 0.5 sig) sig2) (scu/lfo-kr 0.5 -1 1))
                   reson (-> sig2
                             (o/delay-n 0.01 0.01)
                             (o/bpf rz-freq (scu/lfo-kr 3 0.1 0.5))
                             #_(o/free-verb (scu/lfo-kr 2 0.4 0.7)
                                            (scu/lfo-kr 5 1 2)
                                            (scu/lfo-kr 3 0 1))
                             (* 4)
                             (#(o/pan-az 4 % (scu/lfo-kr 5 -1 1))))]
               (-> (+ sig2* reson
                      #_(-> sig2
                            (o/comb-l 0.2 (scu/lfo-kr 1 0.15 0.2) 1)
                            (o/pitch-shift 0.1 ps2 0 0.0001)
                            (* 0.5 (o/env-gen (o/env-perc 2 1  0.2 0.7)))))
                   (o/free-verb (scu/lfo-kr 2 0.4 0.7) 2 0)
                   (* (o/lag amp (o/rand 1 2))
                      (o/env-gen (o/envelope [0 (* 0.7 e1) e1 e2 e3 0.6 0.25 0]
                                             [0.1 0.2 0.5 1.5 4 6 4])
                                 :time-scale time-scale
                                 :action o/FREE))
                   (o/limiter (o/db->amp -6) 0.05)))))

(defn ps-ringz** [params]
  (timbre/info "###" :ps-ringz/actual-call "###")
  (ps-ringz* params))

(def ps-ringz (throttle2 ps-ringz** 100))

(defn mic-1-amp-trig-handler
  [{:keys [args]}]
  (timbre/info :trig/mic-1)
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (+ (rrand -0.2 0.2)
                         (* (rand-nth [200 250 300])
                            (rand-nth [1 2 3 4 #_5])))
             :amp @ps-ringz-amp
             :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))

(defn mic-2-amp-trig-handler
  [{:keys [args]}]
  (timbre/info :trig/mic-2)
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (+ (rrand -0.2 0.2)
                         (* (rand-nth [200 250 300])
                            (rand-nth [2 3 7/2 #_5])))
             :amp @ps-ringz-amp
             ;; :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))

(defn mic-3-amp-trig-handler
  [{:keys [_in args]}]
  (timbre/info :trig/mic-3)
  (play-sample {:group (groups/mid)
                :out mixed-main-out})
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (+ (rrand -0.2 0.2)
                         (* (rand-nth [200 250 299])
                            (rand-nth [2 3 4 #_5 11/8])))
             :amp @ps-ringz-amp
             ;; :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))
(comment
  (ps-ringz {:group (ringz-group)
             :in (-> @inputs :mic-3 :bus)
             :ps1 3/2
             :ps2 (* (rand-nth [1 8/7 11/8 7/4]) (rand-nth [1/4 11/32 4/11 13/32]))
             :rz-freq (* (rand-nth [200 251 300])
                         (rand-nth [1 2 3 4 #_5]))
             :amp @ps-ringz-amp
             :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))

(defn guitar-amp-trig-handler
  [{:keys [in args]}]
  (timbre/info :trig/guitar)
  (play-sample {:group (groups/mid)
                :out mixed-main-out}))

(comment
  (when @habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {})
    (reset! amp-trig/handlers {}))
  (init! {:add-custom-groups-fn add-ringz-group})

  (do ;; Amp triggers
    ;; NOTE IMPORTANT do not forget to add the `add-ringz-group` `:add-custom-groups-fn`
    ;; to the `init!` function's config

    ;; prevent doubling of synths
    (declare mic-1-ampt mic-2-ampt mic-3-ampt guitar-ampt ps-ringz-4ins*)
    (try (o/kill ps-ringz-4ins*) (amp-trig/dereg-handler mic-1-ampt) (amp-trig/dereg-handler mic-2-ampt) (amp-trig/dereg-handler mic-3-ampt) (amp-trig/dereg-handler guitar-ampt) (catch Exception _ nil))

    (def ps-ringz-4ins-bus (o/audio-bus 1 "ps-ringz-4ins-bus"))
    ;; FIXME TODO work on calibrating this with milo (params :mic-<x>-amp and :guitar-amp)
    (def ps-ringz-4ins* (ps-ringz-4ins {:group (ringz-group :head)
                                        :mic-1 (-> @inputs :mic-1 :bus)
                                        :mic-2 (-> @inputs :mic-2 :bus)
                                        :mic-3 (-> @inputs :mic-3 :bus)
                                        :guitar (-> @inputs :guitar :bus)
                                        :guitar-amp 0.9
                                        :out ps-ringz-4ins-bus}))

    ;; NOTE when checking triggers. Make sure to review the levels on the mic group and not on the mic input.
    ;; `:thresh` seems approximative. Sometimes off by ~0.8db.
    ;; Probably because of the way the amplitude is measured.
    (def mic-1-ampt (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                                      :thresh (o/db->amp -30)
                                      :handler #'mic-1-amp-trig-handler
                                      :handler-args {:in-bus ps-ringz-4ins-bus}}))
    (def mic-2-ampt (reg-amp-trigger {:in (-> @inputs :mic-2 :bus)
                                      :thresh (o/db->amp -42)
                                      :handler #'mic-2-amp-trig-handler
                                      :handler-args {:in-bus ps-ringz-4ins-bus}}))
    (def mic-3-ampt (reg-amp-trigger {:in (-> @inputs :mic-3 :bus)
                                      :thresh (o/db->amp -42)
                                      :handler #'mic-3-amp-trig-handler
                                      :handler-args {:in-bus ps-ringz-4ins-bus}}))
    (def guitar-ampt (reg-amp-trigger {:in (-> @inputs :guitar :bus)
                                       :handler #'guitar-amp-trig-handler
                                       :thresh (o/db->amp -25)
                                       :handler-args {:in-bus ps-ringz-4ins-bus}})))
  ;; Amp trigger controls

  (o/ctl ps-ringz-4ins* :guitar-amp 2)
  (o/ctl mic-1-ampt :thresh (o/db->amp -30))
  (o/ctl mic-2-ampt :thresh (o/db->amp -30))
  (o/ctl mic-3-ampt :thresh (o/db->amp -30))
  (o/ctl guitar-ampt :thresh (o/db->amp -25))

  ;; Amp regulator
  (reset! ps-ringz-amp-reg-scale 2)
  (reset! ps-ringz-amp-reg-thresh (o/db->amp -20)) ;; perhaps -20 is good, need to test more

  (reset! log-amp-peak-db? true)
  (reset! log-amp-peak-db? false)

  (do
    ;; prevent doubling of synths
    (declare amp-reg-ins ar)
    (try (o/kill amp-reg-ins) (o/kill ar) (catch Exception _ nil))

    (def amp-regulator-ins-bus (o/audio-bus 4 "amp-regulator-ins-bus"))
    (def amp-reg-ins (amp-regulator-ins {:group (groups/fx)
                                         :guitar (:guitar main-returns)
                                         :percussion (:percussion main-returns)
                                         :guitar-processes (:guitar-processes main-returns)
                                         :percussion-processes (:percussion-processes main-returns)
                                         :mixed (:mixed main-returns)
                                         :non-recordable (:non-recordable main-returns)
                                         :out amp-regulator-ins-bus}))
    (init-amp-regulator-receiver!)

    (def ar (amp-regulator-replier
              (groups/fx)
              :in amp-regulator-ins-bus
              :replyRate 5
              :peakLag 1)))

  (open-inputs-with-rand-pan
    {:inputs inputs
     :preouts preouts})

  (start-rec-loop3!
    {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2 :mic-3]) vals (->> (map :bus))))
     :durs (mapv (fn [_] (rrange 10 20)) (range 40))
     :rec-input-config {:print-info? false}}))
