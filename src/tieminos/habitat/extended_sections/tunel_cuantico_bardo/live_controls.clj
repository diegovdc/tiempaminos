(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :refer [round]]
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.harmonies.chords :refer [meta-slendro1
                                                                rate-chord-seq]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.clouds :refer [clouds-refrain]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :as bardo.rec]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.math.bezier-samples :as bzs]
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

(defn start-recording [{:keys [input-k]}]
  (timbre/info "starting rec on" input-k)
  (if-let [input-bus (-> @inputs input-k :bus)]
    (bardo.rec/start-rec-loop!
     {:id (make-rec-id input-k)
      :input-bus input-bus
      :rec-dur (fn [_] (-> @live-state :rec input-k :dur))
      :rec-pulse (fn [_] (-> @live-state :rec input-k get-rec-pulse))
       ;; :print-info? true
      :on-rec-start (fn [_]
                      (println "starting rec")
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
  [index lorentz]
  (let [min* -18
        max* 18
        lor-speed 50
        index* (* lor-speed index)]
    [(round (lorentz/bound (lorentz index*) :x min* max*))
     (round (lorentz/bound (lorentz index*) :y min* max*))
     (round (lorentz/bound (lorentz index*) :z min* max*))]))

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
     ;; TODO simplify
     :buf-fn (fn [_]
               (let [lib-size (-> @live-state :algo-2.2.9-clouds player-k :sample-lib-size)
                     [_k buf] (->> @rec/bufs
                                  (sort-by (comp :rec/time second))
                                  reverse
                                  (filter (fn [[k _]]
                                            (str/includes? (name k)
                                                           (if (= player-k :milo)
                                                             "mic-"
                                                             "guitar-"))))
                                  (take lib-size)
                                  (#(when (seq %) (rand-nth %))))]
                 #_(println "get buf" k  (into {} buf))
                 buf))
     :rates-fn (fn [{:keys [index]}]
                 (->> (lorentz-chord index (:lorentz @live-state))
                      (#(rate-chord-seq meta-slendro1 [%]))
                      first))
     :amp-fn (fn [_] (-> @live-state :algo-2.2.9-clouds player-k :amp (o/db->amp)))
     :on-play (fn [{:as config :keys [index]}]
                (let [state @live-state]
                  (println "=========" config)
                  (amanecer*guitar-clouds
                    (-> config
                        (merge (get-envelope
                                 index
                                 (-> state :algo-2.2.9-clouds player-k :env)
                                 (:lorentz state)))))))}))

(comment
  (-> @live-state :algo-2.2.9-clouds :milo :amp (o/db->amp))
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
