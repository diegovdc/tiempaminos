(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :refer [round]]
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.harmonies.chords :refer [fib-21
                                                                meta-pelog
                                                                meta-pelog-11
                                                                meta-pelog-7
                                                                meta-slendro-5
                                                                meta-slendro1
                                                                rate-chord-seq]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.clouds :refer [clouds-refrain]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.core :as bardo.gusano]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :as bardo.rec]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management :as bardo.synth-management]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs main-returns]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.overtone-extensions :as oe]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]))

;;;;;;;;;;;;;;
;; Recording
;;;;;;;;;;;;;;

(defn- make-rec-id [input-k]
  (keyword "bardo.rec-loop" (name input-k)))

(defn get-rec-pulse [{:keys [dur pulse]}]
  (case pulse
    :dur*2 (* 2 dur)
    :rand-2 (+ dur (rand dur))
    :rand-4 (+ dur (rand (* 3 dur)))
    dur))

(comment
  (-> @live-state :rec :mic-1 :dur)
  (def input-k :mic-2))

(defn start-recording [{:keys [input-k]}]
  (timbre/info "starting rec on" input-k)
  (if-let [input-bus (-> @inputs input-k :bus)]
    (bardo.rec/start-rec-loop!
     {:id (make-rec-id input-k)
      :input-k input-k
      :input-bus input-bus
      :rec-dur-fn (fn [_]
                    (-> @live-state :rec input-k :dur))
      :rec-pulse (fn [_] (-> @live-state :rec input-k get-rec-pulse))
      ;; :print-info? true
      :on-rec-start (fn [_]
                      (swap! live-state
                             assoc-in
                             [:rec input-k :last-rec-timestamp]
                             (o/now)))})
    (timbre/error "No input bus for key:" input-k)))

(comment
  (gp/stop)
  (reset! rec/recording? {})
  (start-recording {:input-k :mic-1})
  (->> @rec/bufs
       vals
       (map :duration)
       frequencies))

(defn stop-recording [{:keys [input-k]}]
  (timbre/info "stopping rec on" input-k)
  (gp/stop (make-rec-id input-k)))

;;;;;;;;;;;;;;;;;;
;; Clouds
;;;;;;;;;;;;;;;;;;

(defn- make-clouds-id [player-k]
  (keyword "bardo.clouds" (name player-k)))

(def rit (bzs/f 20 0.5 10))
(def accel (bzs/s 20 0.5 10))

(defn get-dur [index rhythm-k lorentz]
  (case rhythm-k
    :lor-0.1_2 (lorentz/bound (lorentz (* 50 index)) :x 0.1 2)
    :lor-2_6 (lorentz/bound (lorentz (* 50 index)) :x 2 6)
    :rand-0_10 (max 0.1 (rand 10))
    :rit (wrap-at index rit)
    :accel (wrap-at index accel)
    (do
      (timbre/error (ex-info "Unknown rhtyhm key, using default"
                             {:rhythm-k rhythm-k}))
      1)))

(defn get-lorentz-envelope
  [index lorentz min* max*]
  {:a (lorentz/bound (lorentz (* 50 index)) :x min* max*)
   :d (lorentz/bound (lorentz (* 50 index)) :y min* max*)
   :r (lorentz/bound (lorentz (* 50 index)) :z min* max*)})

(defn get-envelope [index env-k lorentz]
  (case env-k
    :lor-1_4 (get-lorentz-envelope index lorentz 1 4)
    :lor-0.1_2 (get-lorentz-envelope index lorentz 0.1 2)
    :a-0.1_0.4*d-2*r-3 {:a (rrange 0.1 0.4) :d 2 :r 3}
    :weights-largos {:a (weighted {10 1
                                   15 0.3})
                     :d (weighted {40 1
                                   30 0.3})
                     :r (weighted {10 1
                                   20 0.3})}
    (do
      (timbre/error (ex-info "Unknown envelop key, using default"
                             {:env-k env-k}))
      {:a 3 :d 3 :r 2})))

(defn lorentz-chord
  [index lorentz lor-speed lowest-note highest-note]
  (let [index* (* lor-speed index)]
    [(round (lorentz/bound (lorentz index*) :x lowest-note highest-note))
     (round (lorentz/bound (lorentz index*) :y lowest-note highest-note))
     (round (lorentz/bound (lorentz index*) :z lowest-note highest-note))]))

(oe/defsynth
  cristal-liquidizado
  [buf 0
   rate 1
   amp 0.5
   pan 0
   dur 1
   out 0]
  (o/out out
         (-> (o/play-buf 1 buf rate)
             (* amp
                (o/env-gen
                 (o/envelope
                  [0 1 1 0]
                  [(* 0.1 dur)
                   (* 0.7 dur)
                   (* 0.2 dur)])
                 :action o/FREE))
             (#(o/pan-az:ar 4 % pan)))))

(defn get-harmonic-data!
  [player-k]
  (-> @live-state :algo-2.2.9-clouds player-k))

(defn get-harmony
  [harmony-k]
  (case harmony-k
    :meta-slendro-5 meta-slendro-5
    :meta-slendro-12 meta-slendro1
    :fib fib-21
    :meta-pelog-5 meta-pelog
    :meta-pelog-7 meta-pelog-7
    :meta-pelog-11 meta-pelog-11
    meta-slendro1))

(defn- get-rates-subset
  [rate-indexes rates]
  (keep
   #(nth rates % nil)
   rate-indexes))
#_(get-rates-subset #{0 1 2} [0 1 2])

(defn start-clouds
  [player-k]
  (clouds-refrain
   {:id (make-clouds-id player-k)
    :silence-thresh (o/db->amp -55)
    :durs-fn (fn [{:keys [index]}]
               (let [state @live-state
                     rhythm (-> state :algo-2.2.9-clouds player-k :rhythm)]
                 (get-dur index
                          rhythm
                          (:lorentz state))))
    :buf-fn (fn [_]
              (let [lib-size (-> @live-state :algo-2.2.9-clouds player-k :sample-lib-size)
                    [k buf] (bardo.rec/get-buf
                             player-k
                             lib-size
                             (bardo.live-state/get-active-banks player-k))]
                #_(println "get buf" k  (into {} buf))
                buf))
    :rates-fn (fn [{:keys [index]}]
                (let [{:keys [harmony harmonic-speed harmonic-range
                              rate-indexes ;; defines the number of voices to play, lorentz has 3 indexes so indexes can be a set of numbers 0 - 2
                              ]
                       :or {rate-indexes #{0 1 2}}} (get-harmonic-data! player-k)]
                  (->> (lorentz-chord index
                                      (:lorentz @live-state)
                                      harmonic-speed
                                      (:low harmonic-range)
                                      (:high harmonic-range))
                       (#(rate-chord-seq (get-harmony harmony) [%]))
                       first
                       (get-rates-subset rate-indexes))))
    :amp-fn (fn [_] (-> @live-state :algo-2.2.9-clouds player-k :amp (o/db->amp)))
    :on-play (fn [{:as config :keys [index buf rate]}]
               (let [state @live-state
                     out (main-returns (case player-k
                                         :milo :percussion-processes
                                         :diego :guitar-processes))
                     synth-type (-> state :algo-2.2.9-clouds player-k :active-synth)
                      ;; TODO: update live state with event duration
                     synth (case synth-type
                             :crystal (let [dur (* rate (:duration buf))
                                            synth* (cristal-liquidizado (assoc config :dur dur :out out))]
                                        (bardo.synth-management/add-synth! synth* dur)
                                        synth*)
                             :granular (amanecer*guitar-clouds
                                        (-> config
                                            (merge (get-envelope
                                                    index
                                                    (-> state :algo-2.2.9-clouds player-k :env)
                                                    (:lorentz state)))
                                            (assoc :out out)))
                             (amanecer*guitar-clouds
                              (-> config
                                  (merge (get-envelope
                                          index
                                          (-> state :algo-2.2.9-clouds player-k :env)
                                          (:lorentz state)))
                                  (assoc :out out))))]
                 (swap! bardo.rec/currently-playing-bufs update (:buf config) conj synth)))}))

(comment
  (-> @live-state :algo-2.2.9-clouds :milo)
  (swap! live-state assoc-in [:algo-2.2.9-clouds :milo :rhythm] :lor-0.1_2)
  (o/amp->db 0.0015420217847956035)
  (stop-clouds :milo)
  (start-clouds :milo)
  (stop-clouds :diego)

  (let [player-k :diego
        lib-size
        (-> @live-state :algo-2.2.9-clouds player-k :sample-lib-size)]
    (->> @rec/bufs
         (sort-by (comp :rec/time second))
         reverse
         (filter (fn [[k _]]
                   (str/includes? (name k)
                                  (if (= player-k :milo)
                                    "mic-"
                                    "guitar-"))))
         #_(take lib-size)
         #_(#(when (seq %) (rand-nth %)))))

  (->> @rec/bufs
       (sort-by (comp :rec/time second))
       reverse
       (filter (fn [[k _]]
                 (str/includes? (name k) "mic-")))))

(defn stop-clouds
  [player-k]
  (gp/stop (make-clouds-id player-k)))

(defn start-gusano
  []
  (bardo.gusano/start))

(defn stop-gusano
  []
  (bardo.gusano/stop))
