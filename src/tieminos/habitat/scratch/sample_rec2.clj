(ns tieminos.habitat.scratch.sample-rec2
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.recording :as rec :refer [norm-amp rec-input recording?
                                               silence?]]
   [tieminos.habitat.routing
    :refer [inputs main-returns preouts recordable-outputs]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
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

(defn hacia-un-nuevo-unierso-harmonies
  [{:keys [buf-fn period-dur total-durs loop? refrain-id synth-params]
    :or {period-dur 4
         refrain-id :hacia-un-nuevo-unierso-harmonies
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
  (gp/stop)
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
                        (max 0.01 (- dur-s 0.1)) ;; prevent the `recording?` pred to fail by just a few ms of overlap
                        (fn [_])))))
(defn start-rec-loop2!
  "Calls an input-bus-fn to get a bus for recording."
  [{:keys [input-bus-fn
           durs]
    :or {durs (mapv (fn [_] (rrange 3 10)) (range 40))}}]
  (ref-rain
    :id :rec-loop2
    :durs durs
    :on-event (on-event
                (rec&play (input-bus-fn {:index index})
                          (max 0.01 (- dur-s 0.1)) ;; prevent the `recording?` pred to fail by just a few ms of overlap
                          (fn [_])))))

(defn start-rec-loop3!
  "Calls an input-bus-fn to get a vector or busses for recording"
  [{:keys [input-bus-fn
           durs]
    :or {durs (mapv (fn [_] (rrange 3 10)) (range 40))}}]
  (ref-rain
    :id :rec-loop3
    :durs durs
    :on-event (on-event
                (let [buses (input-bus-fn {:index index})]
                  (doseq [bus buses]
                    (rec&play bus
                              (max 0.01 (- dur-s 0.1)) ;; prevent the `recording?` pred to fail by just a few ms of overlap
                              (fn [_])))))))

(defn rand-latest-buf [_]
  (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 3) (rand-nth)))

(comment
  (open-inputs-with-rand-pan
    {:inputs inputs
     :preouts preouts})
  (gp/stop)
  (reset! recording? {})
  (reset! rec/bufs {})
  (start-rec-loop2!
    {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1]) vals rand-nth :bus))
     :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
  (gp/stop :rec-loop2)

  (start-rec-loop3!
    {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1]) vals (->> (map :bus))))
     :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
  (gp/stop :rec-loop3)

  (hacia-un-nuevo-universo-perc-refrain {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
                                         :rates (map #(* 2 %) [1 6 7 11 9 2 12 8 5 13]) #_(range 1 10)
                                         :amp 1
                                         :period 10
                                         :durs [2 3 5 3]
                                         :d-weights {8 1
                                                     5 1
                                                     3 1}
                                         :d-level-weights {0.3 5
                                                           0.1 2
                                                           0.2 3
                                                           0.4 2}
                                         :a-weights {(rrange 0.01 0.3) 2
                                                     (rrange 1 2) 3
                                                     (rrange 2 5) 1}}))

(defn hacia-un-nuevo-universo-perc-refrain
  [{:keys [buf-fn period durs rates amp d-weights d-level-weights a-weights room-weights]
    :or {buf-fn rand-latest-buf
         period 2.5
         durs (bzs/fsf 20 0.1 1)
         rates (range 1 10)
         amp 1
         a-weights {(rrange 0.01 0.1) 10
                    (rrange 2 5) 1/2}
         d-weights {(rrange 0.2 0.3) 5
                    (rrange 0.3 0.5) 3
                    (rrange 0.5 1) 1
                    (rrange 1 5) 1/2}
         d-level-weights {0.3 1}
         room-weights {0.2 2, 2 1/2 4 1/2}}}]
  (ref-rain
   :id :hacia-un-nuevo-universo-perc2
   :durs (periodize-durs period durs)
   :on-event (on-event
              (when-let [buf (buf-fn {:index index})]
                (when-not (silence? buf)
                  (let [start 0 #_(rrange (rrange 0 0.5) 0.7)
                        end 1 #_(+ start (rrange 0.05 0.3))
                        rate (at-i rates
                                   #_(concat [1 2 3 4 5 6 7 8 9]
                                             (map #(/ % 9/2) (range 9 18))
                                             (map #(/ (* 2 %) 19) (range 19 38))))
                        a (weighted a-weights)
                        trig-rate (+ 90 (rand-int 20))
                        config {:buf buf
                                :a a
                                :d (/ (+ (/ a 2) (weighted d-weights))
                                      2)
                                :r (+ (/ a 2) (weighted d-weights))
                                :d-level (weighted d-level-weights)
                                :rev-room (weighted room-weights)
                                :trig-rate 100
                                :grain-dur (/ 1 (/ trig-rate 2))
                                :amp-lfo (rrange 0.1 0.4)
                                :amp-lfo-min 0.95
                                :lpf-max (rrange 2000 10000)
                                :start start
                                :end end
                                :pan (rrange -1 1)
                                :out (main-returns :non-recordable)}]
                    ;; TODO perhaps add :interp to the `grain-buf` in this synth
                    (amanecer*guitar-clouds (assoc config
                                                   :rate rate
                                                   :interp (rand-nth [1 2 4])
                                                   :amp (* amp (rrange 0.9 1) (norm-amp buf))))
                    (amanecer*guitar-clouds (assoc config
                                                   :rate (* (rand-nth [2 3/2 7/4 1/2 1 1 1 1]) rate)
                                                   :interp (rand-nth [1 2 4])
                                                   :amp (* amp (rrange 0.7 0.9) (norm-amp buf))))))))))

(defn hacia-un-nuevo-universo-perc-refrain-v1p2
  "This version can handle rate chords (as a vector of rates)"
  [{:keys [buf-fn period durs rates amp d-weights d-level-weights a-weights room-weights]
    :or {buf-fn rand-latest-buf
         period 2.5
         durs (bzs/fsf 20 0.1 1)
         rates (range 1 10)
         amp 1
         a-weights {(rrange 0.01 0.1) 10
                    (rrange 2 5) 1/2}
         d-weights {(rrange 0.2 0.3) 5
                    (rrange 0.3 0.5) 3
                    (rrange 0.5 1) 1
                    (rrange 1 5) 1/2}
         d-level-weights {0.3 1}
         room-weights {0.2 2, 2 1/2 4 1/2}}}]
  (let [rates* (map (fn [r] (if (sequential? r) r [r])) rates)]
    (ref-rain
      :id :hacia-un-nuevo-universo-perc2
      :durs (periodize-durs period durs)
      :on-event (on-event
                  (when-let [buf (buf-fn {:index index})]
                    (when-not (silence? buf) ;; allow us to control silences by not playing
                      (let [rate (at-i rates*)]
                        (doseq [r rate]
                          (let [start 0 #_(rrange (rrange 0 0.5) 0.7)
                                end 1 #_(+ start (rrange 0.05 0.3))
                                a (weighted a-weights)
                                trig-rate (+ 90 (rand-int 20))
                                config {:buf buf
                                        :a a
                                        :d (/ (+ (/ a 2) (weighted d-weights))
                                              2)
                                        :r (+ (/ a 2) (weighted d-weights))
                                        :d-level (weighted d-level-weights)
                                        :rev-room (weighted room-weights)
                                        :trig-rate 100
                                        :grain-dur (/ 1 (/ trig-rate 2))
                                        :amp-lfo (rrange 0.1 0.4)
                                        :amp-lfo-min 0.95
                                        :lpf-max (rrange 2000 10000)
                                        :start start
                                        :end end
                                        :out (main-returns :non-recordable)
                                        :pan (rrange -1 1)}]
                            (amanecer*guitar-clouds (assoc config

                                                           :rate (float r)
                                                           :interp (rand-nth [1 2 4])
                                                           :amp (* amp (rrange 0.2 1) (norm-amp buf))))
                            (amanecer*guitar-clouds (assoc config
                                                           :rate (* (rand-nth [2 3/2 5/4 7/4 1/2 1 1 1 1]) r)
                                                           :interp (rand-nth [4])
                                                           :amp (* amp (rrange 0 0.7) (norm-amp buf)))))))))))))


(oe/defsynth amanecer*guitar-clouds-2
  ;; TODO pass in manual envelope
  [buf 0
   trig-rate 40
   grain-dur 1/20
   rate 1
   amp 1
   amp-lfo-min 0.5
   amp-lfo 0.1
   start 0.1
   end 0.3
   a 0.1
   d 1
   d-level 0.3
   r 3
   out 0
   lpf-min 100
   lpf-max 2000
   pan 0
   rev-mix 1
   rev-room 0.5
   a-level 1]
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
             (o/lpf (lfo 0.1 lpf-min lpf-max))
             #_(#(+ % (* 0.7 (o/b-moog % 500 (lfo 2 0.2 0.6) 2))))
             (#(o/pan-az 4 % :pos pan :width (lfo 0.1 1 2.5)))
             (o/free-verb rev-mix rev-room)
             (* amp
                #_(lfo amp-lfo amp-lfo-min 1)
                (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                       [-1 -5])
                           :action o/FREE)))))

(defn hacia-un-nuevo-universo-perc-refrain-2
  [{:keys [buf-fn period durs rates amp d-weights d-level-weights a-weights room-weights]
    :or {buf-fn rand-latest-buf
         period 2.5
         durs (bzs/fsf 20 0.1 1)
         rates (range 1 10)
         amp 1
         a-weights {(rrange 0.01 0.1) 10
                    (rrange 2 5) 1/2}
         d-weights {(rrange 0.2 0.3) 5
                    (rrange 0.3 0.5) 3
                    (rrange 0.5 1) 1
                    (rrange 1 5) 1/2}
         d-level-weights {0.3 1}
         room-weights {0.2 2
                       0.5, 2
                       2 1/2
                       10 1/4}}}]
  (ref-rain
   :id :hacia-un-nuevo-universo-perc2.2
   :durs (periodize-durs period durs)
   :on-event (on-event
              (when-let [buf (buf-fn {:index index})]
                (let [start #_0 (rrange (rrange 0 0.5) 0.7)
                      end #_1 (+ start (rrange 0.05 (- 1 start)))
                      rate (at-i rates)
                      a (weighted a-weights)
                      trig-rate (+ 50 (rand-int 50))
                      config {:buf buf
                              :a a
                              :d (/ (+ (/ a 2) (weighted d-weights))
                                    2)
                              :r (+ (/ a 2) (weighted d-weights))
                              :d-level (weighted d-level-weights)
                              :rev-room (weighted room-weights)
                              :trig-rate trig-rate
                              :grain-dur (/ 1 (/ trig-rate 2))
                              :amp-lfo (rrange 0.1 4)
                              :amp-lfo-min 0.95
                              :lpf-min 20
                              :lpf-max (rrange 2000 10000)
                              :start start
                              :end end
                              :pan (rrange -1 1)
                              :out (main-returns :non-recordable)}]
                  (amanecer*guitar-clouds-2 (assoc config
                                                   :rate rate
                                                   :amp (* amp (rrange 0.9 1) (norm-amp buf))))
                  (amanecer*guitar-clouds-2 (assoc config
                                                   :rate (* (rand-nth [2 3/2 7/4 1/2 1 1 1 1]) rate)
                                                   :amp (* amp (rrange 0.7 0.9) (norm-amp buf)))))))))

(comment
  (hacia-un-nuevo-universo-perc-refrain-2
    {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
     :rates (map #(* 2 %) [1 6 7 11 9 2 12 8 5 13]) #_(range 1 10)
     :amp 0.9
     :period 10
     :durs [2 3 5 3]
     :d-weights {8 1
                 5 1
                 3 1}
     :d-level-weights {0.3 5
                       0.1 2
                       0.2 3
                       0.4 2}
     :a-weights {(rrange 0.01 0.3) 2
                 (rrange 1 2) 3
                 (rrange 2 5) 1}})
  (->> @rec/bufs vals last)

  (gp/stop)
  (ref-rain
    :id :hacia-un-nuevo-universo
    :durs (periodize-durs 20 (bzs/f 20 0.1 1))
    :on-event (on-event
                (let [d (rrange 1 4)
                      buf (rand-latest-buf)
                      config {:buf buf
                              :d d
                              :amp 0.05
                              :rev-room 4
                              :trig-rate 200
                              :grain-dur 1/100
                              :a 2
                              :amp-lfo 20
                              :start 0 :end 1}
                      rates (take (rand-int 5) [1 3 5 7 9])]
                  (amanecer*guitar-clouds (assoc config
                                                 :rate 2
                                                 :amp (* (rrange 0.9 1) (norm-amp buf))
                                                 :pan (rrange -1 1))))))
  (ref-rain
    :id :hacia-un-nuevo-universo-perc
    :durs (periodize-durs 2 (bzs/f 20 0.1 1))
    :on-event (on-event
                (let [buf (rand-latest-buf)
                      config {:buf buf
                              :d (rrange 0.2 0.3)
                              :amp 0.05
                              :rev-room 0.2
                              :trig-rate 100
                              :grain-dur 1/50
                              :a (rrange 0.01 0.1)
                              :amp-lfo 20
                              :start 0 :end 1}
                      rate (at-i [1 2 3 4 5 6 7 8 9])]
                  (amanecer*guitar-clouds (assoc config
                                                 :rate rate
                                                 :amp (* (rrange 0.9 1) (norm-amp buf))
                                                 :pan (rrange -1 1))))))

  #_(gp/stop)

  #_(hacia-un-nuevo-universo-perc-refrain {:amp 1})
  (defn hacia-un-nuevo-universo-perc-refrain
    [{:keys [period notes-in-period durs rates amp d-weights room-weights]
      :or {period 2.5
           notes-in-period 20
           durs (bzs/fsf notes-in-period 0.1 1)
           rates (range 1 10)
           amp 1
           d-weights {(rrange 0.2 0.3) 5
                      (rrange 0.3 0.5) 3
                      (rrange 0.5 1) 1
                      (rrange 1 5) 1/2}
           room-weights {0.2 10, 2 1/2}}}]
    (ref-rain
      :id :hacia-un-nuevo-universo-perc2
      :durs (periodize-durs period durs)
      :on-event (on-event
                  (let [buf (rand-latest-buf) #_(->> @rec/bufs vals last)
                        start (rrange (rrange 0 0.5) 0.7)
                        end (+ start (rrange 0.05 0.3))
                        rate (at-i rates #_(concat [1 2 3 4 5 6 7 8 9]
                                                   (map #(/ % 9/2) (range 9 18))
                                                   (map #(/ (* 2 %) 19) (range 19 38))))
                        a (weighted {(rrange 0.01 0.1) 10
                                     (rrange 2 5) 1/2})
                        config {:buf buf
                                :a a
                                :d (+ (/ a 2) (weighted d-weights))
                                :rev-room (weighted room-weights)
                                :trig-rate 100
                                :grain-dur 1/50
                                :amp-lfo 20
                                :start start
                                :end end
                                :pan (rrange -1 1)}]
                    (amanecer*guitar-clouds (assoc config
                                                   :rate rate
                                                   :amp (* amp (rrange 0.9 3) (norm-amp buf))))
                    (amanecer*guitar-clouds (assoc config
                                                   :rate (* (rand-nth [2 3/2 7/4 1/2]) rate)
                                                   :amp (* amp (rrange 0.7 2) (norm-amp buf)))))))))

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
  (->> @rec/bufs vals (map :analysis))
  (o/demo (o/play-buf 1 (-> @rec/bufs vals rand-nth)))
  (reset! rec/bufs {})
  (let [scale (->> (cps/make 2 [1 3 5 7 9]) :scale)]
    (ref-rain
     :id ::rising-upwards
     :durs (periodize-durs 20 (bzs/f 20 0.1 1))
     :on-event (on-event
                (println i (-> @rec/bufs vals rand-nth))
                (let [buf (-> @rec/bufs vals rand-nth)]

                  (s1 (default-synth-params
                        buf
                        {:amp (* (rrange 0.2 1) (norm-amp buf))
                         :rate (scale/deg->freq scale 1 (at-i [1 1 1 4 6 5]))})))))))
