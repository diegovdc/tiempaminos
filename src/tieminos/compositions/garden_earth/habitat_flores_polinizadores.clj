(ns tieminos.compositions.garden-earth.habitat-flores-polinizadores
  (:require
   [clojure.set :as set]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base
    :refer [dur->env
            early-g
            eik
            interval-from-note
            interval-from-pitch-class
            midi-in-event
            on-event
            pc-index
            ref-rain
            seconds->dur
            stop
            subcps]]
   [tieminos.compositions.garden-earth.init
    :refer [init-groups-and-fx! load-test-samples!]]
   [tieminos.compositions.garden-earth.synths.granular
    :as gs
    :refer [sample&hold]]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [freq-history lfo lfo-kr start-signal-analyzer]]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.osc.reaper :as reaper]
   [tieminos.overtone-extensions :as oe]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.standard :refer [rrand]]))

(require '[clojure.string :as str]
         '[clojure.data.generators :as gen]
         '[tieminos.math.bezier :as bz]
         '[tieminos.math.utils :refer [linexp linlin]])

(o/env-gen (o/env-perc))

(defn most-frequent-val [coll]
  (->> coll frequencies
       (sort-by second >)
       first
       first))

(defn most-frequent-pitch
  "Will try to get the most frequent pitch-class in a `freq-history` segment.
  Might just return a random frequency if all pitch-classes in a segment appear
  an equal number of times.
  This is a heuristic function and might not be accurate enough.
  `freq-history` comes from `erv-fib-synth.compositions.garden-earth.synths.live-signal`"
  [freq-history start-time end-time]
  (let [freq-range  (->> freq-history
                         (take-while #(>= (:timestamp %) start-time))
                         (drop-while #(> (:timestamp %)  end-time)))
        most-frequent-pc (->> freq-range
                              (map :pitch-class)
                              most-frequent-val)
        most-frequent-transp (-> (group-by :pitch-class freq-range)
                                 (get most-frequent-pc)
                                 (->> (map :transp))
                                 most-frequent-val)]
    {:pitch-class most-frequent-pc
     :transp most-frequent-transp}))

(most-frequent-pitch @freq-history 1639060141963 (o/now))

(defonce s&h-seq (atom nil))

(defn fsf
  ([steps] (fsf steps 0.01 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur
           (bz/curve steps
                     [(+ 5 (rand-int 10)) 0.1 1 (+ 5 (rand-int 20)) 7 0.1]))))

(defn f
  ([steps] (f steps  0.03 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur (bz/curve steps [(rand-nth [20 12 5]) 0.1 0.1 6 1]))))

(defn s
  ([steps] (s steps  0.03 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur (bz/curve steps [0.1 (rand-nth [1 5 10])]))))

(def bz {:fsf #'fsf :f #'f :s #'s})

(defn play-sample
  [{:keys [buf] :as _s&h-data}
   & {:keys [amp adr-env grain-conf d-level rate]
      :or {rate 1
           amp 2
           d-level 0.3
           grain-conf {:trig-rate 100 :grain-dur 1/10 :start 0 :end 1}
           adr-env (dur->env {:a 2 :d 2 :r 5} 3)}}]
  (apply sample&hold :buf buf :d-level d-level :amp amp :rate rate
         (merge grain-conf adr-env)))

(def envs {:atk-reso1 {:a 0.1 :d 0.5 :r 5}
           :atk-reso2 {:a 0.1 :d 0.5 :r 0.5}
           :reso1 {:a 0.1 :d 5 :r 1}
           :homo {:a 0.5 :d 0.5 :r 0.5}})

(defn rand-val [m] (val (rand-nth (seq m))))

(do
  (defn on-sample-end
    [play-fn rec-start-time bufs-atom buf-key]
    #_(println "RUN PLAY FN")
    (let [b (@bufs-atom buf-key)
          now (o/now)
          {:keys [pitch-class transp]} (most-frequent-pitch @freq-history
                                                            rec-start-time
                                                            now)
          new-buf-key (into [pitch-class] buf-key)
          s&h-data {:buf b
                    :buf-key new-buf-key
                    :pitch-class pitch-class
                    :transp transp
                    :recording/start-time rec-start-time
                    :recording/end-time now}]
      (when true #_pitch-class
            (play-fn s&h-data)
            ;; TODO rename key to include analyzed "dominant" pitch
            (swap! bufs-atom set/rename-keys {buf-key new-buf-key})
            (swap! s&h-seq conj s&h-data))))
  (comment
    (s&h rec/bufs 0.5 1)))

(defn s&h
  [bufs-atom dur index
   & {:keys [key-prefix play-fn input-bus]
      :or {key-prefix :polinizaflowers
           play-fn #(play-sample
                     %
                     :amp 2
                     :adr-env (dur->env {:a 2 :d 2 :r 5} 2))}}]
  (let [start-time (o/now)]
    (rec/start-recording :input-bus input-bus
                         :bufs-atom bufs-atom
                         :buf-key [key-prefix index]
                         :seconds dur
                         :msg "Rec: Sample&Hold"
                         :on-end (partial on-sample-end
                                          play-fn
                                          start-time
                                          bufs-atom)
                         :countdown 3)))
(comment
  (->> eik :subcps keys (filter #(str/includes? % "2)4")))
  (subcps "2)4 of 3)6 11-1.5.7.9")
  (midi-in-event
   :note-on
   (fn [ev]
     (let [{:as s&h-data
            :keys [pitch-class]} (->> @s&h-seq first)
           ;; scale (:scale eik)
           scale (subcps "2)4 of 3)6 1-5.7.9.11")
           pc-index* (pc-index scale pitch-class)
           rate (interval-from-pitch-class scale pitch-class
                                           (- (:note ev) 60))
           env (envs :atk-reso1)]
       (println pc-index* (:note ev)
                rate)
       (play-sample s&h-data
                    :rate rate
                    :amp (* 8 (/ (ev :velocity) 40))
                    :d-level 0.1
                    :adr-env (dur->env {:a 0.2 :d 2 :r 2} (gen/weighted {3 10 1 1})))))
   :note-off (fn [_] #_(println "off" ((juxt :channel :note) _)))))

(do (defn degree-sequence
      "`direction` is one of #{:asc :desc :opening :closing}"
      [direction interval-size seq-size]
      (let [opening #(take seq-size
                           (interleave
                            (range 0
                                   (* -1 interval-size seq-size)
                                   (* -1 interval-size))
                            (range interval-size
                                   (* interval-size seq-size)
                                   interval-size)))]
        (case direction
          :asc (range 0 (* interval-size seq-size) interval-size)
          :desc (range 0 (* -1 interval-size seq-size) (* -1 interval-size))
          :opening (opening)
          :closing (reverse (opening)))))
    (degree-sequence :closing 3 10)
    (defn interval-sequence
      "`direction` is one of #{:asc :desc :opening :closing}"
      [{:keys [scale direction interval-size seq-size]}]
      (let [deg-seq (degree-sequence direction interval-size seq-size)
            starting-note (rand-nth scale)]
        (map (partial interval-from-note
                      scale
                      starting-note)
             deg-seq)))

    (interval-sequence
     {:scale (subcps "2)4 of 3)6 11-1.5.7.9")
      :direction :closing
      :interval-size 3
      :seq-size 10}))

(do
  (defn s&h-reponse-1
    [{:as s&h-data :keys [pitch-class]}]
    (when pitch-class
      (let [scale #_(:scale eik) (subcps "2)4 of 3)6 11-1.5.7.9")
            pc-index* (pc-index scale pitch-class)
            direction (rand-nth [1 -1])
            intervals (map #(interval-from-pitch-class scale pitch-class %)
                           (map #(* % direction)
                                (range pc-index* (+ 9 (rand-int 7) pc-index*) 2)))
            durs ((rand-val bz)
                  (count intervals)
                  (rand-nth [0.01 0.1 0.3])
                  (rand-nth [0.17 0.5 0.8]))
            env-durs ((rand-val bz) (count intervals) 3 5)
            env (rand-val envs)
            amps (fsf (count intervals) 1 2)
            d-level ((rand-val bz) (count intervals) 0.1 0.3)]
        (ref-rain :id (keyword "s&h" (str "response1-" (rand-int 5000)))
                  :durs durs
                  :loop? false
                  :on-event
                  (on-event
                   (play-sample s&h-data
                                :rate (at-index intervals)
                                :d-level (at-index d-level)
                                :amp (at-index amps)
                                :adr-env (dur->env env (at-index env-durs)))))))))

(do

  (oe/defsynth gr-flor
    [buf 0
     dur 1
     trig-rate 20
     grain-dur 1/5
     rate 1
     amp 1
     min-amp 0.5
     amp-lfo 0.1
     start 0
     end 1
     a 0.1
     d 1
     d-level 0.3
     r 3
     out 0
     pan 0]

    (o/out out
           (let [sig (o/grain-buf
                      :num-channels 4
                      :trigger (o/dust trig-rate)
                      :dur [grain-dur]
                      :sndbuf buf
                      :rate rate
                      :pos (o/line start end (+ a d r))
                      :pan pan)]
             (-> sig
                 (+
                  #_(* (lfo 0.5 0.1 1)
                       (o/free-verb
                        (+ (o/play-buf 1 buf (lfo 8 0.99 1.01))
                           (o/play-buf 1 buf (lfo 8 0.99 1.01)))
                        1 0.2 0.8))
                  (* (lfo 5 0.3 5)
                     (o/rlpf sig
                             (lfo (lfo 2 0.5 1.5)
                                  300 1000)
                             (lfo (lfo 2 0.5 1.5)
                                  0.3 0.7))))
                 #_(o/lpf 7000)
                 #_(o/free-verb 1 0.3 0.8)
                 (* amp (lfo 1 0.3 1)
                    (o/env-gen (o/envelope [0 0.8 d-level 0] [a d r]
                                           [-1 -5])
                               :action o/FREE))))))
  (oe/defsynth gr-flor-2
    [buf 0
     dur 1
     trig-rate 100
     grain-dur 1/10
     rate 1
     amp 1
     min-amp 0.5
     amp-lfo 0.1
     start 0.1
     end 0.3
     a 0.1
     d 1
     d-level 0.3
     r 3
     out 0]

    (o/out out
           (let [sig (o/grain-buf
                      :num-channels 4
                      :trigger (o/dust (lfo-kr 20 (* 0.7 trig-rate)
                                               (* 1.3 trig-rate)))
                      :dur [grain-dur]
                      :sndbuf buf
                      :rate rate
                      :pos (o/line start end (+ a d r))
                      :pan (lfo 0.1 -1 1))]
             (-> (+
                  (* (lfo 1.3 0.7 1)
                     (o/rlpf sig
                             (lfo (lfo 2 0.5 1.5)
                                  100 1000)
                             (lfo 2 0.3 0.7)))
                  (* (lfo 10 0.7 2.5)
                     (o/rhpf sig
                             (lfo (lfo 3 0.5 1.5)
                                  30 60)
                             (lfo 2 0.1 0.3))))
                 (o/free-verb (lfo 0.5 0.1 1)
                              (lfo 10 0.1 1)
                              (lfo 2 0.3 1))
                 (* amp 3
                    (o/env-gen (o/envelope [0 0.8 d-level 0] [a d r]
                                           [-1 -5])
                               :action o/FREE))
                 (o/distort)
                 (o/distort)
                 (* 0.5)))))
  (oe/defsynth flor
    [buf 0
     dur 1
     trig-rate 100
     grain-dur 1/10
     rate 1
     amp 1
     min-amp 0.5
     amp-lfo 0.1
     start 0.1
     end 0.3
     a 0.1
     d 1
     d-level 0.3
     r 3
     out 0]

    (o/out out
           (let [sig (o/play-buf 1 buf (/ rate  2))]
             (-> sig
                 #_(+ (* (lfo 0.5 0.1 0.7)
                         (o/rlpf sig
                                 (lfo (lfo 2 0.5 1.5)
                                      300 10000)
                                 (lfo 2 0.3 1))))
                 #_(o/lpf 7000)
                 (* amp
                    (o/env-gen (o/envelope [0 1 d-level 0] [a d r]
                                           [-1 -5])
                               :action o/FREE))))))

  (defn florains-1
    [{:as s&h-data :keys [buf]}]
    (let [scale (->> eik :subcps vals (filter #(>= (count (:scale %)) 3)) (rand-nth) :scale)
          seq-size (+ 3 (rand-int 15))
          pan (rrand -1.0 1)
          general-amp 1]
      (ref-rain :id (keyword (str "florains-1_id" (rand-int 5000)))
                :loop? false
                :durs ((rand-nth [f s fsf]) seq-size 0.1 0.3)
                :ratio 1
                :on-event
                (on-event
                 (let [dur-amp (rrand 0.01 1)
                       rate (* 1/16 (rand-nth (map :bounded-ratio scale) #_[1 3/2 7/5 7/4])) #_(nth intervals index 1)
                       start (rrand 0.3 0.5)
                       end (rrand 0.5 0.6)
                       [a d r] (shuffle [(* 3/6 dur-amp (:duration buf))
                                         (* 1/6 dur-amp (:duration buf))
                                         (* 1/3 dur-amp (:duration buf))])]
                   #_(println "scale-size" (count scale) "interval" (nth intervals index 1)
                              (* dur-amp (:duration buf)))

                   (gr-flor
                    :group (early-g)
                    :out 8
                    :buf buf
                    :a (* a)
                    :d d
                    :r r
                    :d-level (rand-nth [0.2 0.5])
                    :rate rate
                    :pan (rrand -1 1)
                    :start start
                    :end end
                    :amp (* general-amp #_(rand-nth [1.2 0.3 0.7 0.5])
                              ;; lower amp based on distance from 1/1
                            #_(/ 1 (+ 1 (* (rrand 1 2) (Math/abs (float (- 1 rate))))))))
                   #_(gr-flor
                      :group (early-g)
                      :out 8
                      :buf buf
                      :a a
                      :d d
                      :r r
                      :d-level 0.3
                      :rate 1
                      :pan pan
                      :start start
                      :end end
                      :amp (* 2 general-amp)))))))
  (comment
    (florains-1 {:buf (spy (rand-nth test-samples*))})))

(comment

  (o/recording-start "~/Desktop/flores.wav")
  (o/recording-stop)
  (defn spy [x] (println x) x)
  (start-signal-analyzer)
  (-> @freq-history)
  (def test-samples (load-test-samples!))
  (def test-samples* (-> @rec/bufs vals vec))
  (-> test-samples*)
  (-> @rec/bufs)
  (reset! rec/bufs {})
  (gs/grain :buf (spy (test-samples* 67))
            :amp 5)
  (-> @rec/bufs)
  (o/sample-player (-> @rec/bufs first second)
                   :amp 10)

  ((o/synth (o/out 0 (* 5 (o/play-buf 1 (-> @rec/bufs first second))))))
  (stop)
  (s&h rec/bufs 0.5 1 :input-bus 0 :play-fn s&h-reponse-1))

(defn polinizadores-nocturnos
  []
  (ref-rain :id :polinizadores-nocturnos-1
            :durs (shuffle [6 6.5 8 6 7 5 8])
            :ratio 1
            :on-event (on-event
                       (when (> 0.5 (rand))
                         (s&h rec/bufs
                              (+ 0.5 (rand 5))
                              index
                              ;; FIXME
                              :input-bus (rand-nth [0 2])
                              :play-fn (fn [_])))))
  (ref-rain :id :polinizadores-nocturnos-2
            :durs (shuffle [6 6.5 8 6 7 5 7])
            :ratio 1
            :on-event (on-event
                       (when (> 0.5 (rand))
                         (s&h rec/bufs
                              (+ 0.5 (rand 5))
                              index
                              :input-bus (rand-nth [1 3])
                              :play-fn (fn [_])))))
  (ref-rain :id :florainer
            :durs (fsf 10 2 5)
            :ratio 1
            :on-event (on-event
                       (let [buf (let [bufs (into [] @rec/bufs)]
                                   (when (not (zero? (count bufs)))
                                     (-> bufs rand-nth second)))]
                         (when buf
                           (florains-1 {:buf buf}))))))

#_(ref-rain :id :florainer
            :durs (fsf 10 2 5)
            :ratio 1
            :on-event (on-event
                       (let [buf (let [bufs (into [] @rec/bufs)]
                                   (when (not (zero? (count bufs)))
                                     (-> bufs rand-nth second)))]
                         (when buf
                           (florains-1 {:buf buf})))))

(def sections
  [[(* 60 54) #(do (reaper/play)
                   #_(init-groups-and-fx!)
                   (reset! rec/bufs {})
                   (timbre/info "Start"))]
   [(* 60 5) (fn [] (polinizadores-nocturnos))]

   [1 (fn []
        (timbre/info "Ends Polinizadores Nocturnos")
        (stop :polinizadores-nocturnos-1)
        (stop :polinizadores-nocturnos-2)
        (stop :florainer))]])

(def bpm 60)
(defn habitat-player [sections & {:keys [start-index] :or {start-index 0}}]
  (let [sections*   (map #(update-in % [0] seconds->dur bpm)
                         sections)
        durs (map first sections*)]
    (ref-rain :id ::main
              :durs durs
              :loop? false
              :tempo 60
              :on-event (on-event ((-> sections
                                       (nth (+ start-index index))
                                       second))))))
(comment
  (reaper/init)
  (reaper/play)
  (reaper/stop)
  (init-groups-and-fx!)
  (stop)
  (keys @gp/refrains)
  (reset! gp/refrains {})
  (habitat-player sections))
