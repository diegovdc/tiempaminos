(ns tieminos.compositions.garden-earth.fl-grain-1.sample&hold
  (:require
   [clojure.data.generators :as gen]
   [clojure.set :as set]
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base
    :refer [dur->env eik interval-from-pitch-class midi-in-event on-event
            pc-index ref-rain stop subcps]]
   [tieminos.compositions.garden-earth.synths.granular
    :refer [sample&hold]]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [freq-history pan-verb]]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.math.bezier :as bz]
   [tieminos.math.utils :refer [linexp]]))


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

(comment

  (stop)
  (o/recording-start "/home/diego/Desktop/razgado2.wav")
  (o/recording-stop))

(do
  (defn on-sample-end
    [play-fn rec-start-time bufs-atom buf-key]
    (println "on-sample end" play-fn)
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
      (when pitch-class
        (play-fn s&h-data)
        ;; TODO rename key to include analyzed "dominant" pitch
        (println s&h-data)
        (swap! bufs-atom set/rename-keys {buf-key new-buf-key})
        (swap! s&h-seq conj s&h-data))))
  (comment
    (s&h rec/bufs 0.5 1)))

(defn s&h [bufs-atom dur index
           & {:keys [in play-fn]
              :or {in 0
                   play-fn #(play-sample
                              %
                              :amp 2
                              :adr-env (dur->env {:a 2 :d 2 :r 5} 2))}}]
  (let [start-time (o/now)]
    (rec/start-recording {:input-bus in
                          :bufs-atom bufs-atom
                          :buf-key [:sample&hold index]
                          :seconds dur
                          :msg "Rec: Sample&Hold"
                          :on-end (partial on-sample-end
                                           play-fn
                                           start-time bufs-atom)
                          :countdown 1})))
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

(do
  (defn s&h-reponse-1
    [{:as s&h-data :keys [pitch-class]}]
    (println s&h-data)
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

(comment
  (stop)
  (pan-verb :in 5 :mix 1 :room 1 :amp 2 :damp-min 0.6 :damp 0.7
            :pan-min -0.5 :pan 0.5)
  ;; NOTE `ge-live-sig/start-signal-analyzer' should be running
  (s&h rec/bufs 0.5 1 {:in 5 :play-fn s&h-reponse-1})
  (ref-rain :id :s&h-rain
            :durs [5 3 8 2 1 5]
            :ratio 1/3
            :on-event (on-event (s&h rec/bufs 0.5 index
                                     {:in 5 :play-fn s&h-reponse-1}))))
