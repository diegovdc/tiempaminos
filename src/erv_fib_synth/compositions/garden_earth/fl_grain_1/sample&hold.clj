(ns erv-fib-synth.compositions.garden-earth.fl-grain-1.sample&hold
  (:require
   [clojure.set :as set]
   [erv-fib-synth.compositions.garden-earth.base
    :refer [dur->env interval-from-pitch-class on-event pc-index ref-rain stop]]
   [erv-fib-synth.compositions.garden-earth.synths.granular
    :refer [sample&hold]]
   [erv-fib-synth.compositions.garden-earth.synths.live-signal
    :refer [freq-history]]
   [erv-fib-synth.compositions.garden-earth.synths.recording :as rec]
   [overtone.core :as o]))

(do
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

  (->  @freq-history)
  (most-frequent-pitch @freq-history 1639060141963 (o/now)))

(defonce s&h-seq (atom nil))

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

(comment
  ;; TODO left here -- figure out interpolation curves for durs and amps
  (let [{:as s&h-data
         :keys [pitch-class]} (->> @s&h-seq
                                   (filter #(-> % :buf :id (= 71)))
                                   first)
        scale (:scale eik)
        pc-index* (pc-index scale pitch-class)
        intervals (map #(interval-from-pitch-class scale pitch-class % )
                       (range pc-index* (+ 10 pc-index*) 2))
        durs (repeat (count intervals) 0.1)]
    (println pitch-class intervals)
    (ref-rain :id (keyword "s&h" "hola")
              :durs durs
              :loop? false
              :on-event
              (on-event
               (play-sample s&h-data
                            :rate (at-index intervals)
                            :d-level 0.1
                            :adr-env (dur->env {:a 0.1 :d 0.5 :r 5} 5)))))
  (stop))


(do
  (defn on-sample-end
    [play-fn rec-start-time bufs-atom buf-key]
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
      (play-fn s&h-data)
      ;; TODO rename key to include analyzed "dominant" pitch
      (println s&h-data)
      (swap! bufs-atom set/rename-keys {buf-key new-buf-key})
      (swap! s&h-seq conj s&h-data )))
  (comment
    (s&h rec/bufs 0.5 1)))


(defn s&h [bufs-atom dur index]
  (let [start-time (o/now)]
    (rec/start-recording :bufs-atom bufs-atom
                         :buf-key [:sample&hold index]
                         :seconds dur
                         :msg "Rec: Sample&Hold"
                         :on-end (partial on-sample-end
                                          #(play-sample
                                            %
                                            :amp 2
                                            :adr-env (dur->env {:a 2 :d 2 :r 5} 2))
                                          start-time bufs-atom)
                         :countdown 1)))
