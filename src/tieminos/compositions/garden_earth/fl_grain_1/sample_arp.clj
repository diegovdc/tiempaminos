(ns tieminos.compositions.garden-earth.fl-grain-1.sample-arp
  (:require
   [clojure.data.generators :as gen]
   [clojure.set :as set]
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base
    :refer [dur->env eik interval-from-pitch-class interval-from-pitch-class2
            midi-in-event on-event pc-index ref-rain stop subcps]]
   [tieminos.compositions.garden-earth.synths.granular
    :refer [sample&hold]]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [freq-history pan-verb]]
   [tieminos.math.bezier :as bz]
   [tieminos.math.utils :refer [linexp]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.recording.v1 :as sc.rec.v1]))

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
                                 (get most-frequent-pc) (->> (map :transp))
                                 most-frequent-val)]
    {:pitch-class most-frequent-pc
     :transp most-frequent-transp}))

(most-frequent-pitch @freq-history 1639060141963 (o/now))

(defonce arp-seq (atom nil))

(defn fsf
  ([steps] (fsf steps 0.01 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur
           (bz/curve steps
                     [15 1 0.1 (rand-nth [26 20]) 7 0.1]))))

(defn f
  ([steps] (f steps  0.03 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur (bz/curve steps [(rand-nth [20 12 5]) 0.1 0.1 6 1]))))

(defn s
  ([steps] (s steps  0.03 0.1))
  ([steps min-dur max-dur]
   (linexp min-dur max-dur (bz/curve steps [0.1 (rand-nth [1 5 10])]))))

(def bz {:fsf fsf :f f :s s})

(defn play-sample
  [{:keys [buf] :as _arp-data}
   & {:keys [out amp adr-env grain-conf d-level rate]
      :or {out 0
           rate 1
           amp 2
           d-level 0.3
           grain-conf {:trig-rate 100 :grain-dur 1/10 :start 0 :end 1}
           adr-env (dur->env {:a 2 :d 2 :r 5} 3)}}]
  (sample&hold
   (merge {:group (groups/mid)
           :out out
           :buf buf
           :d-level d-level
           :amp amp
           :rate rate}
          grain-conf
          adr-env)))

(def envs {:atk-reso1 {:a 0.1 :d 0.5 :r 5}
           :atk-reso2 {:a 0.1 :d 0.5 :r 0.5}
           :reso1 {:a 0.1 :d 5 :r 1}
           :homo {:a 0.5 :d 0.5 :r 0.5}})

(defn rand-val [m] (val (rand-nth (seq m))))

(comment

  (stop)
  (o/recording-start "/home/diego/Desktop/razgado2.wav")
  (o/recording-stop))

(do
  (defn on-sample-end
    [play-fn rec-start-time bufs-atom buf-key]

    (let [b (@bufs-atom buf-key)
          now (o/now)
          {:keys [pitch-class transp]} (most-frequent-pitch @freq-history
                                                            rec-start-time
                                                            now)
          new-buf-key (into [pitch-class] buf-key)
          arp-data {:buf b
                    :buf-key new-buf-key
                    :pitch-class pitch-class
                    :transp transp
                    :recording/start-time rec-start-time
                    :recording/end-time now}]
      (when pitch-class
        (play-fn arp-data)
        ;; TODO rename key to include analyzed "dominant" pitch
        (swap! bufs-atom set/rename-keys {buf-key new-buf-key})
        (swap! arp-seq conj arp-data)))))

(comment
  (arp {:bufs-atom sc.rec.v1/bufs :dur 0.5 :index 1})

  (o/demo
   (o/play-buf 1 (-> @sc.rec.v1/bufs
                     (get ["A+53" :sample-arp 5])))))

(defn arp
  "NOTE `in` should be an `o/audio-bus`"
  [{:keys [bufs-atom dur index ;; these 3 are required
           in play-fn]
    :or {in 0
         play-fn #(play-sample
                   %
                   :amp 2
                   :adr-env (dur->env {:a 2 :d 2 :r 5} 2))}}]
  (let [start-time (o/now)]
    (sc.rec.v1/start-recording
     {:input-bus in
      :bufs-atom bufs-atom
      :buf-key [:sample-arp index]
      :seconds dur
      :msg "Rec: Sample Arp"
      :print-info? false
      :on-end (partial on-sample-end
                       play-fn
                       start-time
                       bufs-atom)
      :countdown 1})))
(comment
  (->> eik :subcps keys (filter #(str/includes? % "2)4")))
  (subcps "2)4 of 3)6 11-1.5.7.9")
  (midi-in-event
   :note-on
   (fn [ev]
     (let [{:as arp-data
            :keys [pitch-class]} (->> @arp-seq first)
           ;; scale (:scale eik)
           scale (subcps "2)4 of 3)6 1-5.7.9.11")
           pc-index* (pc-index scale pitch-class)
           rate (interval-from-pitch-class scale pitch-class
                                           (- (:note ev) 60))
           env (envs :atk-reso1)]
       (println pc-index* (:note ev)
                rate)
       (play-sample arp-data
                    :rate rate
                    :amp (* 8 (/ (ev :velocity) 40))
                    :d-level 0.1
                    :adr-env (dur->env {:a 0.2 :d 2 :r 2} (gen/weighted {3 10 1 1})))))
   :note-off (fn [_] #_(println "off" ((juxt :channel :note) _)))))

(do
  (defn arp-reponse-1
    [{:keys [scale out]
      :or {out 0}}
     {:as arp-data :keys [pitch-class]}]
    (when pitch-class
      (let [pc-index* (pc-index scale pitch-class)
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
        (ref-rain :id (keyword "arp" (str "response1-" (rand-int 5000)))
                  :durs durs
                  :loop? false
                  :on-event
                  (on-event
                   (play-sample arp-data
                                {:out out
                                 :rate (at-index intervals)
                                 :d-level (at-index d-level)
                                 :amp (at-index amps)
                                 :adr-env (dur->env env (at-index env-durs))})))))))

(defn default-interval-seq-fn
  [pitch-class scale]
  (let [direction (rand-nth [1 -1])]
    (map #(interval-from-pitch-class2 scale pitch-class %)
         (map #(* % direction)
              (range 0 (+ 9 (rand-int 7)) 2)))))
(comment

  (default-interval-seq-fn "C+20" (:scale eik))
  (interval-from-pitch-class2 (:scale eik) "C+20" 0))

(defn pattern-interval-seq-fn
  [pattern pitch-class scale]
  (map #(interval-from-pitch-class2 scale pitch-class %)
       pattern))
(do
  (defn arp-reponse-2
    [{:keys [scale out interval-seq-fn
             env-min-dur env-max-dur
             amp-min amp-max]
      :or {out 0
           interval-seq-fn default-interval-seq-fn
           env-min-dur 3
           env-max-dur 5
           amp-min 1
           amp-max 1.5}
      :as _config-data}
     {:as arp-data :keys [pitch-class]}]
    (when pitch-class
      (let [intervals (interval-seq-fn pitch-class scale)
            durs ((rand-val bz)
                  (count intervals)
                  (rand-nth [#_0.01 0.1 0.3])
                  (rand-nth [0.17 0.5 0.8]))
            env-durs ((rand-val bz) (count intervals) env-min-dur env-max-dur)
            env (rand-val envs)
            amps (fsf (count intervals) amp-min amp-max)
            d-level ((rand-val bz) (count intervals) 0.1 0.3)]
        (ref-rain :id (keyword "arp" (str "response1-" (rand-int 5000)))
                  :durs durs
                  :loop? false
                  :on-event
                  (on-event
                   (play-sample arp-data
                                {:out out
                                 :rate (at-index intervals)
                                 :d-level (at-index d-level)
                                 :amp (at-index amps)
                                 :adr-env (dur->env env (at-index env-durs))})))))))

(comment
  (stop)
  (pan-verb :in 5 :mix 1 :room 1 :amp 2 :damp-min 0.6 :damp 0.7
            :pan-min -0.5 :pan 0.5)
  ;; NOTE `ge-live-sig/start-signal-analyzer' should be running
  (arp {:bufs-atom sc.rec.v1/bufs
        :dur 0.5
        :index 1
        :in 5 :play-fn arp-reponse-1})
  (ref-rain :id :arp-rain
            :durs [5 3 8 2 1 5]
            :ratio 1/3
            :on-event (on-event (arp {:bufs-atom sc.rec.v1/bufs
                                      :dur 0.5
                                      :index index
                                      :in 5
                                      :play-fn (partial arp-reponse-1 "2)4 of 3)6 11-1.5.7.9")}))))
