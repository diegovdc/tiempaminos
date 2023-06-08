(ns tieminos.habitat.scratch.sample-rec2
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.recording :as rec :refer [norm-amp rec-input]]
   [tieminos.habitat.routing
    :refer [inputs main-returns mixed-main-out recordable-outputs]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn bus [k] (-> @inputs k :bus))
(defn outbus [k] (-> main-returns k))

(oe/defsynth recordable-output
  [bus 0 out 0]
  (o/out out (o/mix (o/in bus 4))))

(defn make-recorable-outputs
  [output-buses]
  (->> output-buses
       (mapv (fn [[k bus-num]]
               (let [bus (o/audio-bus 1 (str (name k) "-recordable-output-bus"))]
                 [k {:bus bus
                     :synth (recordable-output {:group (groups/preouts :tail)
                                                :bus bus-num
                                                :out bus})}])))
       (into {})))

(defn rec&play [bus dur-s on-end]
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name (:name bus)
              :input-bus bus
              :dur-s dur-s
              :on-end on-end}))

(oe/defsynth s1
  [buf 0
   amp 1
   a 0.1
   s 2
   r 0.5
   start-pos 0
   room 2
   rate 1
   out 0]
  (o/out out (-> (o/play-buf 1 buf :rate rate :start-pos start-pos)
                 (#(o/pan-az 4 % (lfo 0.2 -1 1)))
                 (o/free-verb 0.8 room)
                 (* amp (o/env-gen (o/envelope [0 1 1 0] [a s r])
                                   :action o/FREE)))))

(do
  (defn periodize-durs
    [period durs]
    (let [total-dur (apply + durs)
          dur-ratio (/ period total-dur)]
      (map #(* % dur-ratio) durs)))
  (periodize-durs 10 (bzs/f 20 0.1 1)))

(defn default-synth-params
  ([buf] (default-synth-params buf {}))
  ([buf params]
   (merge {:buf buf
           :start-pos (-> buf :n-samples rand-int)
           :a (rrange 0.01 0.1)
           :s (rrange 2 10)
           :r (rrange 1 3)
           :amp (* 0.9 (norm-amp bus))
           :room (rrange 0.5 2)
           :out (main-returns :non-recordable)}
          params)))

(defn rising-upwards
  [{:keys [buf-fn period-dur total-durs loop? refrain-id synth-params]
    :or {period-dur 4
         refrain-id :rising-upwards
         total-durs 20
         loop? true}}]
  (let [buf (buf-fn {})]
    (when buf
      (ref-rain
       :id refrain-id
       :durs (periodize-durs period-dur (bzs/f total-durs 0.1 1))
       :loop? loop?
       :on-event (on-event
                  (s1 (default-synth-params
                        buf
                        (synth-params {:i i :buf buf}))))))))

(comment
  (-> @rec/bufs)
  (let [scale (->> (cps/make 3 [9 13 15 21 27 31]) :scale)]
    (rising-upwards
     {:buf-fn (fn [_] (-> @rec/bufs vals rand-nth))
      :period-dur 20
      :total-durs 20
      :loop? true
      :refrain-id :rising-upwards-loop
      :synth-params (fn [{:keys [buf i]}]
                      {:amp (* (rrange 0.2 1) (norm-amp buf))
                       :rate (scale/deg->freq scale 1 (+ (mod i 43)))})})))

(defn start-rec-loop!
  []
  (ref-rain
   :id :rec-loop
   :durs (mapv (fn [_] (rrange 3 10)) (range 40))
   :on-event (on-event
              (rec&play (-> @recordable-outputs vec rand-nth second :bus)
                        dur-s
                        (fn [_])))))

(comment
  (init!)
  (timbre/set-level! :info)
  (let [buf (-> @rec/bufs vals rand-nth)]
    (o/demo
     (* (norm-amp buf)
        (o/play-buf 1 buf))))
  (-> @rec/bufs first second :amp-norm-mult)
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "guitar"
              :input-bus (bus :guitar)
              :dur-s (rrange 2 5)})
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "mixed"
              :input-bus (outbus :mixed)
              :dur-s (rrange 1 5)})
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "mic-1"
              :input-bus (bus :mic-1)
              :dur-s (rrange 1 5)})

  (s1 {:buf (-> @rec/bufs vals rand-nth)
       :start-pos (-> @rec/bufs vals rand-nth :n-samples rand-int)})
  (reset! rec/bufs {})
  (-> @rec/bufs vals rand-nth :n-samples rand-int)
  (gp/stop ::sample-rec)
  (map int (bzs/fsf 50 1 30))

  (dedupe (map int (bzs/fsf 100 1 30)))
  (let [cps (->> (cps/make 3 [9 13 15 21 27])
                 cps/+all-subcps
                 :subcps
                 (filter (fn [[_k cps]] (-> cps :scale count (= 6))))
                 vec
                 rand-nth
                 second)
        scale (:scale cps)
        graph (-> cps :graphs :full)]
    (rand-nth (vec (graph  (rand-nth scale)))))

  (->> scale (wrap-at 10) :bounded-ratio)
  (let [cps (->> (cps/make 3 [9 13 15 21 27])
                 cps/+all-subcps
                 :subcps
                 (filter (fn [[_k cps]] (-> cps :scale count (= 6))))
                 vec
                 rand-nth
                 second)
        scale (:scale cps)
        graph (-> cps :graphs :full)
        prev-note (atom (rand-nth scale))]
    (ref-rain
     :id ::graph-navigation
     :durs (periodize-durs 20 (bzs/f 6 0.5 1))
     :on-event (on-event
                (let [buf (-> @rec/bufs vals rand-nth)]
                  (s1 (synth-params
                       buf
                       {:start-pos 0
                        :s 2
                        :r 10
                        :amp (* (rrange 1 2) (norm-amp buf))
                        :room 5
                        :rate (* (rand-nth [1 1/2 2]) (:bounded-ratio @prev-note))})))
                (swap! prev-note #(rand-nth (vec (graph %)))))))

  (gp/stop ::graph-navigation)

  (gp/stop ::rising-upwards)

  (rec&play (-> @recordable-outputs :mixed :bus)
            5
            (let [i* (rand-int 20)]
              (fn [buf-key]
                (rising-upwards
                 {:scale (->> (cps/make 3 [9 13 15 21 27 31]) :scale)
                  :buf-fn (fn [_] (get @rec/bufs buf-key))
                  :period-dur 40
                  :total-durs 20
                  :loop? false
                  :refrain-id (keyword "rising-upwards" (name buf-key))
                  :synth-params {:amp 1
                                 :rate-fn (fn [{:keys [scale i]}] (scale/deg->freq scale 1 (+ i* (mod i 43))))}}))))

  (keys @rec/bufs)
  (norm-amp (get @rec/bufs :amanecer-ide-mixed-39))
  (rising-upwards
   {:scale (->> (cps/make 3 [9 13 15 21 27 31]) :scale)
    :buf-fn (fn [_] (-> @rec/bufs vals rand-nth))
    :period-dur 20
    :total-durs 20
    :loop? true
    :refrain-id :rising-upwards-loop
    :synth-params (fn [{:keys [buf i]}]
                    {:amp (* (rrange 0.2 1) (norm-amp buf))
                     :rate (fn [{:keys [scale i]}] (scale/deg->freq scale 1 (+ (mod i 43))))})})
  (gp/stop)
  (->> @rec/bufs  vals (map :amp-norm-mult))
  (reset! rec/bufs {})
  (let [scale (->> (cps/make 3 [9 13 15 21 27 31]) :scale)]
    (ref-rain
     :id ::rising-upwards
     :durs (periodize-durs 20 (bzs/f 20 0.1 1))
     :on-event (on-event
                (let [buf (-> @rec/bufs vals rand-nth)]

                  (s1 (synth-params
                       buf
                       {:amp (* (rrange 0.2 1) (norm-amp buf))
                        :rate (scale/deg->freq scale 1 (+ (mod i 43)))})))))))
