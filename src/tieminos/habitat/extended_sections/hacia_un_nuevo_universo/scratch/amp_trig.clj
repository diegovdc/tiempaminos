(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trig
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.amp-trigger :as amp-trig :refer [reg-amp-trigger]]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scsyndefs.core :refer [amp-regulator-replier]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.osc :refer [args->map]]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.habitat.panners :refer [panner panner-rate stop-panner!]]
   [tieminos.habitat.recording :as rec :refer [silence?]]
   [tieminos.habitat.routing :refer [get-input-bus inputs main-returns
                                     mixed-main-out percussion-processes-main-out
                                     preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [amanecer*guitar-clouds-2
                                                 start-rec-loop3!]]
   [tieminos.math.utils :refer [hyperbolic-decay]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :as scu]
   [tieminos.utils :refer [iter-async-call2 rrange throttle2 wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]
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

(defonce play-sample? (atom false))
(defn play-sample
  [{:keys [out]}]
  (when play-sample?
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
                                   :pan (rrange -1 1)})))))

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
   ps1 1
   ps2 1
   rz-freq 200
   amp 1
   time-scale 1
   e1 1
   e2 0.8
   e3 0.65
   out 0]
  (o/out out (let [sig (o/in in 1)
                   sig2 (+ (-> sig (o/pitch-shift 0.05 ps2))
                           (-> sig (o/pitch-shift 0.05 ps1)
                               (* 0.8)))
                   sig2* (o/pan-az 4 (+ (* 0.25 sig) sig2) (scu/lfo-kr 0.5 -1 1))
                   reson (-> (+ sig2 (* 0.25 sig))
                             (o/bpf rz-freq (scu/lfo-kr 3 0.25 0.5))
                             (* 2.5)
                             (#(o/pan-az 4 % (scu/lfo-kr 5 -1 1))))]
               (-> (+ sig2*
                      reson
                      #_(-> sig2
                            (o/comb-l 0.2 (scu/lfo-kr 1 0.15 0.2) 1)
                            (o/pitch-shift 0.1 ps2 0 0.0001)
                            (* 0.5 (o/env-gen (o/env-perc 2 1  0.2 0.7)))))
                   (o/free-verb (scu/lfo-kr 2 0.4 0.7) 2 0)
                   (* (o/lag amp (o/rand 0.5 1))
                      (o/env-gen (o/envelope [0 (* 0.7 e1) e1 e2 e3 0.6 0.25 0]
                                             [0.1 0.2 0.5 1.5 4 6 4])
                                 :time-scale time-scale
                                 :action o/FREE))
                   (o/limiter (o/db->amp -6) 0.05)))))

(defn ps-ringz** [params]
  (timbre/info "###" :ps-ringz/actual-call "###")
  (ps-ringz* params))

(def ps-ringz (throttle2 ps-ringz** 100))

(defn- rand-ratio [ratios]
  (if (map? ratios)
    (weighted ratios)
    (rand-nth ratios)))

(defonce ps2-ratios-index (atom 0))
(-> @ps2-ratios-index)

(defn- get-ps-ratio!
  []
  (let [index @ps2-ratios-index
        cross-sets
        [[[8/7 11/8 7/4] [1/4 11/32 4/11 13/32]] ;; original
         [[8/7 33/28 7/4] [1/4  4/11 4/7]]       ;; original v2
         [[9/7 4/7 7/4] [1/4 4/7 7/10]]          ;; jade
         [[5/4 5/3 25/11] [1/4 7/10 13/8]]       ;; brillante azul
         [[5/4 5/3 25/11 8/13] [1/4 7/10]]       ;; brillante azul v2
         [[5/4 5/3 25/11] [1/4  7/10 13/8] {1/2 2 1 1}] ;; brillante azul v3 (golondrina)
         [[5/4 5/13 25/11] [1/4 7/10 3/2]] ;; golondrina v2 (prime rotation A)
         [[5/4 5/13 25/11] [1/4 7/10 3/2] [7]] ;; golondrina v2.1 (prime rotation A)
         [[13/16 13/24 13/11 1/22] [1/4 11/26 33/64]] {2 3 1 1}] ;; púrpura
        ]
    (->> cross-sets
         (wrap-at index)
         (map rand-ratio)
         (apply *))))

(defn mic-1-amp-trig-handler
  [{:keys [args]}]
  (timbre/info :trig/mic-1)
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (get-ps-ratio!)
             :rz-freq (+ (rrand -0.2 0.2)
                         (* (rand-nth [200 250 300])
                            (rand-nth [1 2 3 4 5])))
             :amp @ps-ringz-amp
             ;; :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))

(defn mic-2-amp-trig-handler
  [{:keys [args]}]
  (timbre/info :trig/mic-2)
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (get-ps-ratio!)
             :rz-freq (+ (rrand -0.2 0.2)
                         (* (rand-nth [200 250 300])
                            (rand-nth [2 3 7/2 5])))
             :amp @ps-ringz-amp
             :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))

(defn mic-3-amp-trig-handler
  [{:keys [_in args]}]
  (timbre/info :trig/mic-3 args)
  (play-sample {:group (groups/mid) ;; only when `play-sample?' is true
                :out mixed-main-out})
  (ps-ringz {:group (ringz-group)
             :in (:in-bus args)
             :ps1 3/2
             :ps2 (get-ps-ratio!)
             :rz-freq (+ (rrand -0.2 0.2)
                         (* (rand-nth [200 250 299])
                            (rand-nth [2 3 4 5 11/8])))
             :amp @ps-ringz-amp
             :time-scale (rand-nth [1/2 1 3/2])
             :out percussion-processes-main-out}))

(comment
  (mic-3-amp-trig-handler {:args {:in-bus (-> @inputs first second)}}))

(defn guitar-amp-trig-handler
  [{:keys [in args]}]
  (timbre/info :trig/guitar)
  (play-sample {:group (groups/mid) ;; only when `play-sample?' is true
                :out mixed-main-out}))

(defn open-inputs-with-rand-pan*
  "`inputs` and `preouts` are atoms.
  `config` is a map where the key is the input key and the value is a `panner` config"
  [{:keys [inputs preouts] :as _context}
   config]
  (doseq [input @inputs]
    (let [[k {:keys [bus]}] input
          rate* (:rate (k config))
          rate (or rate* (rrange 0.1 0.5))]
      (panner (merge {:in bus
                      :type :rand
                      :out (:bus (k @preouts))
                      ;; TODO is amp something that is wanted?
                      :amp 0.5}
                     (k config)))
      (panner-rate (merge {:in bus
                           :rate rate
                           :max 1})))))

(defn stop-panned-inputs!
  []
  (doseq [[k] @inputs]
    (stop-panner! (get-input-bus k))))

(comment
  (get-input-bus :guitar)
  (panner-rate {:in (get-input-bus :guitar)
                :rate (rrange 0.1 0.5)
                :max 0.5}))

(comment)

(defn- slide-fader
  [{:keys [initial-fader-pos
           target-fader-pos
           increment
           wait-on-traget-pos-ms
           update-fader-pos-fn]
    :or {update-fader-pos-fn (fn [pos finished?] (println "slide-fader" pos finished?))}}]
  (iter-async-call2 120 (let [current-fader-pos (atom initial-fader-pos)
                              waiting-timestamp (atom nil)
                              direction (atom :down)]
                          (fn [{:keys [stop-chan-fn]}]
                            (case @direction
                              :down (do (reset! current-fader-pos (max target-fader-pos (- @current-fader-pos increment)))
                                        (update-fader-pos-fn @current-fader-pos false)
                                        (cond
                                          (and (= @current-fader-pos target-fader-pos)
                                               (nil? @waiting-timestamp))
                                          #_{:clj-kondo/ignore [:redundant-do]}
                                          (do (reset! waiting-timestamp (o/now))
                                              #_(println "waiting" @waiting-timestamp)
                                              #_(flush))

                                          (and (= @current-fader-pos target-fader-pos)
                                               (> (- (o/now) wait-on-traget-pos-ms) @waiting-timestamp))
                                          (do (reset! current-fader-pos target-fader-pos)
                                              (reset! direction :up)
                                              (reset! waiting-timestamp (o/now)))
                                          :else nil))

                              :up (do (reset! current-fader-pos (min initial-fader-pos (+ @current-fader-pos increment)))
                                      (update-fader-pos-fn @current-fader-pos false)
                                      #_(println "x" @current-fader-pos)
                                      #_(flush)
                                      (when (= @current-fader-pos initial-fader-pos)
                                        (update-fader-pos-fn @current-fader-pos true)
                                        #_(println "stopping" (- (o/now) @waiting-timestamp))
                                        #_(flush)
                                        (stop-chan-fn))))))))
(let [running? (atom false)]
  (defn lower-mic-group-volume!
    [reaper-osc-client]
    (when-not @running?
      (timbre/info "Lowering mic group volume on Reaper")
      (reset! running? true)
      (slide-fader
       {:initial-fader-pos 0.716 ;; ca.0db
        :target-fader-pos 0.48   ;; ca.-12db
        :increment 0.015
        :wait-on-traget-pos-ms 10000
        :update-fader-pos-fn (fn [pos finished?]
                               (osc/osc-send reaper-osc-client "/track/3/volume" (float pos))
                               (when finished? (reset! running? false)))}))))

(comment
  ;; OSC interactions
  ;; receving port should be 16180
  (def reaper-osc-client (habitat-osc/make-reaper-osc-client))
  (def internal-osc-client (habitat-osc/make-internal-osc-client))
  (defn osc-set-section [n]
    (osc/osc-send internal-osc-client
                  (str "/Sections/section" n)
                  1.0))
  (osc-set-section 1)
  (habitat-osc/responder
   (fn [{:keys [path args] :as msg}]
     (let [args-map (args->map args)
           press? (= 1.0 (first args))]
       (case path
         "/Sections/section1" (when press?
                                (timbre/info "Initing section #1")
                                (open-inputs-with-rand-pan*
                                 {:inputs inputs
                                  :preouts preouts}
                                 {}))
         "/Sections/section2" (when press?
                                (timbre/info "Initing section #2")
                                (open-inputs-with-rand-pan*
                                 {:inputs inputs
                                  :preouts preouts}
                                 {:guitar {:rate (rrange 1 3)}
                                  :mic-1 {:rate (rrange 1 3)}
                                  :mic-2 {:rate (rrange 1 3)}
                                  :mic-3 {:rate (rrange 1 3)}}))
         "/Sections/section3" (when press? (println "INiting section3"))
         "/Sections/play-sample" (if press?
                                   (do
                                     (timbre/info "Starting sampler")
                                     (reset! play-sample? true)
                                     (start-rec-loop3!
                                      {:id ::rec-loop
                                       :input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2 :mic-3]) vals (->> (map :bus))))
                                       :durs (mapv (fn [_] (rrange 10 20)) (range 40))
                                       :rec-input-config {:print-info? false}}))
                                   (do
                                     (timbre/info "Stopping sampler")
                                      ;; NOTE that `::rec-loop` is never stopped
                                     (reset! play-sample? false)))

         "/Sections/ps2-ratios-index" (let [index (first args)]
                                        (if (int? index)
                                          (reset! ps2-ratios-index index)
                                          (timbre/error "Cannot set ps2-ratios-index" {:args args})))
         "/feedback-panic" (when press? (lower-mic-group-volume! reaper-osc-client))
         (timbre/warn "Unknown path for message: " msg args-map)))))
  ;; Init proceedure
  (when @habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {})
    (reset! amp-trig/handlers {}))
  (init! {:add-custom-groups-fn add-ringz-group})

  (open-inputs-with-rand-pan*
   {:inputs inputs
    :preouts preouts}
   {}
   #_{:guitar {:amp 1
               :type :clockwise
               :rate 1}})

  #_(stop-panned-inputs!)

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

  (o/ctl ps-ringz-4ins* :guitar-amp 1.2)
  (o/ctl mic-1-ampt :thresh (o/db->amp -30))
  (o/ctl mic-2-ampt :thresh (o/db->amp -30)) ;; TODO may need to change this
  (o/ctl mic-3-ampt :thresh (o/db->amp -30))
  (o/ctl guitar-ampt :thresh (o/db->amp -25))

  ;; Amp regulator
  (reset! ps-ringz-amp-reg-scale 8)
  (reset! ps-ringz-amp-reg-thresh (o/db->amp -32)) ;; perhaps -20 is good, need to test more

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

  (start-rec-loop3!
   {:id ::rec-loop
    :input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2 :mic-3]) vals (->> (map :bus))))
    :durs (mapv (fn [_] (rrange 10 20)) (range 40))
    :rec-input-config {:print-info? false}})

  (gp/stop ::rec-loop))
