(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.core
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :refer [floor]]
   [erv.scale.core :refer [deg->freq]]
   [erv.utils.core :refer [interval period-reduce]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.harmonies.chords :refer [fib-21
                                                                fib-chord-seq
                                                                transpose-chord]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.gusano-2-2-4 :as g-2.2.4]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.gusano-2-2-6 :as g-2.2.6]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :as rec :refer [norm-amp silence?]]
   [tieminos.habitat.routing :refer [main-returns]]
   [tieminos.habitat.routing :as habitat.route]
   [tieminos.habitat.scratch.sample-rec2 :refer [periodize-durs
                                                 rand-latest-buf]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(def ^:private rates
  [(:rates g-2.2.6/s1)
   (:rates g-2.2.6/s2)
   (:rates g-2.2.6/s3)
   (:rates g-2.2.6/s4)
   (:fib-0.5.13.21-range0.6-5step-interleaved+reverse g-2.2.4/chords)
   (:fib-0.5.13.21-range-3.6-5step-interleaved+reverse g-2.2.4/chords)
   (:fib-multiple-interleaved g-2.2.4/chords)])

(defn get-rates!
  []
  #_(-> @bardo.live-state/live-state :gusano (:rates 0) (wrap-at rates))
  [(nth (wrap-at 0 rates) 2)]
  (wrap-at 0 rates)
  #_(fib-chord-seq (transpose-chord [0 5 13 21] [-9])))

(->> (get-rates!)
     first
     first
     (period-reduce))

(let [prev-index (atom 0)]
  (defn- next-rate-index! [speed]
    (let [prev-i @prev-index]
      (int (floor (reset! prev-index (+ prev-i speed)))))))

(defn- get-rates-seq-speed!
  []
  (-> @bardo.live-state/live-state :gusano (:rates-seq-speed 1)))

(defn- get-amp!
  []
  (-> @bardo.live-state/live-state :gusano (:amp 0.6)))

(def ^:private periods [15 20 25 30 35 40])

(defn- get-period!
  []
  (-> @bardo.live-state/live-state :gusano (:period 0) (wrap-at periods)))

(def ^:private durs
  [[2 3 5 3 8]
   (bzs/fsf 20 0.1 1)
   (fn [] (map (fn [_] (+ 0.0001 (rand))) (range 5)))])

(defn- get-durs!
  []
  (let [durs (-> @bardo.live-state/live-state :gusano (:durs 0) (wrap-at durs))]
    (cond
      (sequential? durs) durs
      (fn? durs) (durs))))

(def ^:private map-trig-rate
  (memoize (fn [trig-rate]
             (first (linlin 0 1 10 110 [trig-rate])))))

(defn- get-trig-rate!
  []
  (-> @bardo.live-state/live-state :gusano (:grain-trig-rate 0.5) map-trig-rate))

(def ^:private map-grain-dur
  (memoize (fn [grain-dur]
             (first (linlin 0 1 10 110 [grain-dur])))))

(defn- get-grain-dur!
  []
  (-> @bardo.live-state/live-state :gusano (:grain-dur 0.5) map-grain-dur))

(defn get-buf!
  [_]
  (timbre/spy :info
              (->> @rec/bufs vals (sort-by :rec/time)
                   reverse
                   (filter (fn [data]
                             (let [has-analysis? (:analysis data)
                                   active-sources (-> @bardo.live-state/live-state :gusano (:sources #{}))
                                   ins (->> (concat
                                             (when (active-sources :diego) habitat.route/diego-ins)
                                             (when (active-sources :milo) habitat.route/milo-ins))
                                            (into #{}))]
                               (if-not (seq active-sources)
                                 has-analysis?
                                 (and has-analysis?
                                      (ins (:input-name (:rec/meta data))))))))
                   (take 3)
                   (#(when (seq %) (rand-nth %))))))

(def fib-ratios-indexes
  (->> fib-21
       (map-indexed (fn [i {:keys [bounded-ratio]}] {bounded-ratio i}))
       (into {})))

(deg->freq fib-21 1 0)

(let [degree-wave (concat (range 0 -37 -4)
                          (range -37 37 4)
                          (range 37 0 -5))]
  (defn second-voice-wave
    [index ratio]
    (let [ratio-index* (fib-ratios-indexes (period-reduce ratio))
          ratio-index (if ratio-index*
                        ratio-index*
                        (do
                          (timbre/error "second-voice-wave: Ratio not found" {:index index :ratio ratio})
                          0))
          interval-deg (wrap-at index degree-wave)
          deg-ratio (deg->freq fib-21 1 (+ ratio-index interval-deg))]
      (* ratio (interval ratio deg-ratio)))))

(defn rand-second-voice
  [index ratio]
  (let [ratio-index* (fib-ratios-indexes (period-reduce ratio))
        ratio-index (if ratio-index*
                      ratio-index*
                      (do
                        (timbre/error "second-voice-wave: Ratio not found" {:index index :ratio ratio})
                        0))
        interval-deg (rrand -21 21)
        deg-ratio (deg->freq fib-21 1 (+ ratio-index interval-deg))]
    (* ratio (interval ratio deg-ratio))))

(def second-voice-fn
  [(fn [_index _ratio] nil)
   (fn [_index ratio] (* (rand-nth [2 3/2 5/4 7/4 1/2 1 1 1 1]) ratio))
   (fn [index ratio] (second-voice-wave index ratio))
   (fn [index ratio] (rand-second-voice index ratio))])

(defn get-second-voice!
  [index ratio]

  (let [second-voice-index (-> @bardo.live-state/live-state :gusano (:second-voice-index 0))
        f (wrap-at second-voice-index second-voice-fn)]
    (f index ratio)))

(def ^:private periodize-durs* (memoize periodize-durs))

(comment
  (periodize-durs
   (get-period!)
   (get-durs!))

  (ref-rain
   :id :x
   :durs (fn [{:keys [index]}] (wrap-at index (periodize-durs* (get-period!) (get-durs!))))
   :on-event (on-event
              (println dur-s)))
  (gp/stop :x))

;; TODO left here
(defn gusano
  "Based on `tieminos.habitat.scratch.sample-rec2/hacia-un-nuevo-universo-perc-refrain-v1p2`
  Can handle rate chords (as a vector of rates)"
  [{:keys [id buf-fn period durs amp
           amp-fn ;; optional, takes the index and returns an amp value, if present `amp` will be overriden
           d-weights d-level-weights a-weights room-weights out-bus silence-thresh
           on-play]
    :or {id :hacia-un-nuevo-universo-perc2
         buf-fn rand-latest-buf
         period 2.5
         durs (bzs/fsf 20 0.1 1)
         amp 1
         a-weights {(rrange 0.05 0.3) 5
                    (rrange 2 5) 1/2}
         d-weights {(rrange 0.2 0.3) 5
                    (rrange 0.3 0.5) 3
                    (rrange 0.5 1) 1
                    (rrange 1 5) 1/2}
         d-level-weights {0.3 1}
         room-weights {0.2 2, 2 1/2 4 1/2}
         out-bus (main-returns :mixed)
         silence-thresh 0.0}}]

  (ref-rain
   :id id
   :durs (fn [{:keys [index]}] (wrap-at index (periodize-durs* (get-period!) (get-durs!))))
   :on-event (on-event

              (when-let [buf (buf-fn {:index index})]
                (when-not (silence? silence-thresh buf) ;; allow us to control silences by not playing
                  (let [rates (get-rates!)
                        rates* (map (fn [r] (if (sequential? r) r [r])) rates)
                        rate (wrap-at (next-rate-index! (get-rates-seq-speed!)) rates*)
                        amp* (get-amp!)]
                    (doseq [r rate]
                      (let [start (rrange (rrange 0 0.1) 0.3)
                            end (min 1 (+ start (rrange 0.05 1)))
                            a (weighted a-weights)
                            trig-rate (+ (get-trig-rate!)
                                         (rrange 0 20))
                            grain-dur (/ 1
                                         (/ (+ (get-grain-dur!) (rand-int 20))
                                            2))
                            config {:group (groups/mid)
                                    :buf buf
                                    :a a
                                    :d (/ (+ (/ a 2) (weighted d-weights))
                                          2)
                                    :r (+ (/ a 2) (weighted d-weights))
                                    :d-level (weighted d-level-weights)
                                    :rev-room (weighted room-weights)
                                    :trig-rate trig-rate
                                    :grain-dur grain-dur
                                    :amp-lfo (rrange 0.1 0.4)
                                    :amp-lfo-min 0.95
                                    :lpf-max (rrange 2000 10000)
                                    :start start
                                    :end end
                                    :out out-bus
                                    :pan (rrange -1 1)}]
                        (when on-play
                          #_(println "ONPLAY")
                          (on-play (assoc config
                                          :amp amp*
                                          :rate (float r))))
                        (timbre/info "Playing-----" (:out config))
                        (amanecer*guitar-clouds (assoc config
                                                       :rate (float r)
                                                       :interp (rand-nth [1 2 4])
                                                       :amp (* amp* (rrange 0.2 1) (norm-amp buf))))
                        (when-let [rate* (get-second-voice! index r)]
                          (amanecer*guitar-clouds (assoc config
                                                         :rate rate*
                                                         :interp 4
                                                         :amp (* amp* (rrange 0 0.7) (norm-amp buf)))))))))))))

(def default-config
  {:on-play (fn [& _] (println "playing"))
   :id ::gusano
   :out-bus (main-returns :mixed)
   :silence-thresh 0.0
   :buf-fn get-buf!
   :rates (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))
                      (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))))
   :amp-fn (fn [_i] 1)
   :period 30
   :durs [2 3 5 3 8]
   :d-weights {(rrand 0.1 1) 1
               8 1
               5 1
               3 2}
   :d-level-weights {0.3 5
                     0.1 2
                     0.2 3
                     0.4 2}
   :a-weights {(rrange 0.01 0.2) 1/4
               (rrange 0.2 0.8) 1
               (rrange 1 2) 3
               (rrange 2 5) 1}})

(defn start
  []
  (gusano default-config))

(comment
  (require '[tieminos.overtone-extensions :as oe])
  (oe/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc 200)))))

  (def test-sini (sini :out (main-returns :mixed)))
  (o/kill test-sini)

  (-> @gp/refrains)
  (start))

(defn stop
  []
  (gp/stop ::gusano))
