(ns tieminos.habitat.reactivity.amp
  (:require
   [clojure.core.async :as async]
   [overtone.music.time :refer [now]]
   [taoensso.timbre :as timbre]
   [tieminos.math.utils :refer [avg linearly-weighted-avg]]))

(defonce amp-analysis
  (atom {::inited? false
         :analize-inputs #{} ;; #{:guitar}
         :data {} ;; i.e. {:guitar '({:amp 0.5 :timestamp 10}, {:amp 0.3 :timestamp 5} ...)}
         :lin-weighted-amps {} ;; i.e. {:guitar {:avg-30s 0.5 :avg-5s 0.4}}
         }))

;; API
(defn add-amp-data [input-key timestamp amp]
  (when (::inited? @amp-analysis)
    (swap! amp-analysis update-in [:data input-key] conj {:timestamp timestamp :amp amp})))

(defn add-bus-to-analysis
  [bus-keyword]
  (swap! amp-analysis update :analize-inputs conj bus-keyword))

(defn remove-bus-from-analysis
  [bus-keyword]
  (swap! amp-analysis update :analize-inputs
         (fn [bus-kw-set] (->> bus-kw-set
                               (remove  #(= % bus-keyword))
                               set))))


;; impl

(def ^:private max-timespan-ms 30000)

(defn- lin-weighted-avg-amp [end-time start-time input-amp-data]
  (let [amps (->> input-amp-data
                  (drop-while #(> (:timestamp %) end-time))
                  (take-while #(>= (:timestamp %) start-time))
                  (map :amp))]
    (linearly-weighted-avg amps)))

(defn- analyse-input-amps
  [input-amp-data]
  (let [now* (now)
        prev-30s (- now* max-timespan-ms)
        prev-5s (- now* 5000)]
    {:avg-30s (lin-weighted-avg-amp now* prev-30s input-amp-data)
     :avg-5s (lin-weighted-avg-amp now* prev-5s input-amp-data)}))

(defn- analyse-amps [amp-analysis-data]
  (->> amp-analysis-data
       (map (fn [[k data]]
              [k (analyse-input-amps data)]))
       (into {})))

(defn- analyze-amps!
  []
  (let [avg-amps (analyse-amps (select-keys (:data @amp-analysis)
                                            (:analize-inputs @amp-analysis)))
        avg-amps-vals (vals avg-amps)
        avg-30s  (->> avg-amps-vals (map :avg-30s) avg)
        avg-5s  (->> avg-amps-vals (map :avg-5s) avg)]
    (swap! amp-analysis assoc :lin-weighted-amps
           (assoc avg-amps
                  :avg-30s (float avg-30s)
                  :avg-5s (float avg-5s)))))

(defn- remove-old-amp-data
  [amp-analysis-data last-timestamp]
  (->> amp-analysis-data
       (map (fn [[k data*]]
              [k (take-while #(> (:timestamp %) last-timestamp)
                             data*)]))
       (into {})))

(comment
  (remove-old-amp-data {:guitar '({:timestamp 8}
                                  {:timestamp 6}
                                  {:timestamp 4}
                                  {:timestamp 3})
                        :guitar2 '({:timestamp 8}
                                   {:timestamp 6}
                                   {:timestamp 4}
                                   {:timestamp 3})}
                       5))

(defn- remove-old-amp-data!
  []
  (let [last-timestamp (- (now) max-timespan-ms)]
    (swap! amp-analysis update :data
           remove-old-amp-data last-timestamp)))

(defn init-amp-analyzer!
  ([] (init-amp-analyzer! 1500))
  ([interval-ms]
   (when-not (::inited? @amp-analysis)
     (timbre/debug "Initializing amp analyzer")
     (let [stop-chan1 (async/chan)
           stop-chan2 (async/chan)
           close-chans-fn! #(do (async/put! stop-chan1 true)
                                (async/put! stop-chan2 true)
                                (swap! amp-analysis assoc
                                       ::inited? false
                                       :data {}
                                       ::stop-fn nil))]

       (swap! amp-analysis assoc
              ::inited? true
              ::stop-fn close-chans-fn!)

       ;; run analisis
       (async/go-loop
           []
           (async/alt!
             (async/timeout interval-ms) (do
                                           (analyze-amps!)
                                           (recur))
             stop-chan1 nil))

       ;; remove old amp data
       (async/go-loop
           []
           (async/alt!
             (async/timeout 1000) (do
                                    (remove-old-amp-data!)
                                    (recur))
             stop-chan2 nil))
       close-chans-fn!))))


(defn stop-amp-analyzer!
  []
  ((::stop-fn @amp-analysis)))

(defn analyze-amps? [input-kw]
  ((:analize-inputs @amp-analysis)
   input-kw))

(comment
  (def analyzer-close-fn (init-amp-analyzer!))
  (analyzer-close-fn)
  (reset! amp-analysis {})
  (add-amp-data :guitar (now) (rand))
  (do
    (doseq [i (reverse (range 100))]
      (add-amp-data :guitar
                    (- (now) (* i 50))
                    (rand)))
    @amp-analysis))
