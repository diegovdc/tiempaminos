(ns tieminos.habitat.recording
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [overtone.core :as o]
   [overtone.music.time :refer [now]]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.reactivity.amp :refer [add-amp-data analyze-amps?]]
   [tieminos.habitat.routing :refer [bus->bus-name input-number->bus*]]
   [tieminos.math.utils :refer [avg]]
   [tieminos.sc-utils.recording.v1 :as rec :refer [start-recording]]
   [tieminos.utils :refer [hz->ms normalize-amp rrange]]))

(declare make-buf-key! add-analysis add-meta)

(defonce bufs (atom {}))

(defonce recording?
  ;;  {:input-name-kw boolean?}
  (atom {}))

(defn rec-input
  [{:keys [section
           subsection
           input-name
           input-bus
           dur-s
           on-end
           msg
           countdown
           on-rec-start
           print-info?
           group]
    :or {on-end (fn [& _args])
         msg "Recording"
         countdown 0
         print-info? true}}]
  (let [input-kw (-> input-bus :name keyword)]
    (if (get @recording? input-kw)
      (timbre/warn "Input bus already recording: " input-kw)
      (do
        (swap! recording? assoc input-kw true)
        (start-recording
         :bufs-atom bufs
         :buf-key (make-buf-key! section subsection input-name)
         :input-bus input-bus
         :seconds dur-s
         :msg msg
         :print-info? print-info?
         :on-end (fn [buf-key]
                   (swap! recording? assoc input-kw false)
                   (add-analysis dur-s buf-key input-bus)
                   (add-meta buf-key section subsection input-name)
                   (on-end buf-key))
         :countdown countdown
         :on-rec-start on-rec-start
         :group group)))))

(def habitat-samples-path (str (System/getProperty "user.dir")
                               "/samples/habitat_samples/"))

(defn save-samples [& {:keys [description full-keyword buffers-atom]}]
  (when-not buffers-atom
    (throw (ex-info "A buffers-atom must be passed in... perhaps try rec/bufs" {})))
  (let [prefix  (or full-keyword
                    (keyword "habitat"
                             (str "*"
                                  (-> (.format (java.time.ZonedDateTime/now)
                                               java.time.format.DateTimeFormatter/ISO_INSTANT))
                                  (when description (str "*" (str/replace description #" " "-"))))))]
    (rec/save-samples prefix
                      :path habitat-samples-path
                      :buffers-atom buffers-atom
                      :preserve-keys [:analysis :amp-norm-mult])))

(defonce buf-keys-count (atom {}))

(defn- make-buf-key! [section subsection input-name]
  (let [partial-name (format "%s-%s-%s" section subsection input-name)
        sample-number (get (swap! buf-keys-count update partial-name #(if % (inc %) 1))
                           partial-name)]
    (keyword (str partial-name "-" sample-number))))

(defn rec-controller
  [{:keys [in section subsection dur-s]
    :as osc-data}]
  (let [input-bus (input-number->bus* in)]
    (if (and input-bus section subsection dur-s)
      (rec-input {:section section
                  :subsection subsection
                  :input-name (bus->bus-name input-bus)
                  :input-bus input-bus
                  :dur-s dur-s})
      (throw (ex-info "Data missing from osc command for recording"
                      {:osc-data osc-data})))))

(defn get-samples-db!
  []
  (edn/read-string (slurp (str habitat-samples-path "db.edn"))))

(comment
  (-> (get-samples-db!)
      :test/gusano-cuantico-2.2.9.2))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus]])
  (-> guitar-bus :name (str/replace #"-bus" ""))
  (rec-input {:section "test"
              :subsection "p1"
              :input-name "guitar"
              :input-bus guitar-bus
              :dur-s 5})

  (save-samples :description "test"))

;;;;;;;;;;;;;;;;;;;
;; Signal Analysis
;;;;;;;;;;;;;;;;;;;

(defonce analysis-history (atom {}))
(comment
  ;; TODO is freq being added to the analysis?
  (->> @analysis-history vals flatten (filter :freq?))
  (->> @analysis-history :guitar-bus (map :freq))
  (->> @analysis-history :mic-1-bus (map :freq)))

(def analyzer-freq 60) ;; TODO lower and test

(defn run-get-signal-analysis
  [& {:keys [freq input-bus analysis-path]
      :or {freq 60}}]
  (timbre/debug "Initializing signal analyzer on bus:" input-bus)
  ((o/synth
    (let [input (o/in input-bus)]
      (o/send-reply (o/impulse freq) analysis-path
                    ;; TODO Probably use PeakFollower instead of amplitude
                    [(o/amplitude:kr input) (o/pitch:kr input)]
                    1)))))

(defn do-receive-analysis
  [input-bus-name-keyword data]
  (let [recording-bus? (get @recording? input-bus-name-keyword)
        analyzing-amp? (analyze-amps? input-bus-name-keyword)]
    (when (or recording-bus? analyzing-amp?)
      (let [[_node-id _input-bus amp freq freq?*] (-> data :args)
            freq? (= freq?* 1.0)]

        (when analyzing-amp?
          (add-amp-data input-bus-name-keyword (now) amp))

        (when recording-bus?
          #_(timbre/debug input-bus-name-keyword "amp" amp "freq" freq)
          (swap! analysis-history
                 update
                 input-bus-name-keyword
                 conj
                 {:amp amp
                  :timestamp (o/now)
                  :freq freq
                  :freq? freq?}))))))

(defn run-receive-analysis
  "Gets analysis from `in` 0 in almost real time
  and `conj`es the data into`analysis-history.
  NOTE: `run-get-signal-pitches` must be running`"
  [& {:keys [freq input-bus-name-keyword analysis-path]}]
  (let [handler-name (keyword (str (str/replace analysis-path #"/" "")
                                   "-handler"))]
    (o/on-event analysis-path
                (fn [data] (do-receive-analysis input-bus-name-keyword data))
                handler-name)
    handler-name))

(comment
  (require '[tieminos.habitat.routing :refer [inputs]])
  (reset! recording? {})
  (-> @inputs :guitar :bus keys)
  (o/demo (o/in (-> @inputs :guitar :bus))))

(defn start-signal-analyzer
  [& {:keys [input-bus]}]
  (let [input-bus-name-keyword (keyword (:name input-bus))
        analysis-path (format "/receive-%s-analysis" (:name input-bus))]
    (timbre/debug "Starting analyzer with path:" analysis-path " for input " input-bus-name-keyword)
    {:receiver (run-receive-analysis
                :freq analyzer-freq
                :input-bus-name-keyword input-bus-name-keyword
                :analysis-path analysis-path)
     :analyzer (run-get-signal-analysis
                :freq analyzer-freq
                :input-bus input-bus
                :analysis-path analysis-path)}))

(defn add-analysis [dur-s buf-key input-bus]
  (let [now (o/now)
        sample-start (- now (* 1000 dur-s)
                        ;; ensure samples window corresponds to dur-s
                        (hz->ms analyzer-freq))
        analysis (->> (get @analysis-history (keyword (:name input-bus)))
                      (drop-while #(> (:timestamp %) now))
                      (take-while #(>= (:timestamp %) sample-start))
                      (reduce (fn [acc {:keys [amp freq freq?]}]
                                (-> acc
                                    (update :min-amp min amp)
                                    (update :max-amp max amp)
                                    (update :amps conj amp)
                                    (cond-> freq? (update :freqs conj freq))))

                              {:min-amp 0
                               :max-amp 0
                               :amps ()
                               :freqs ()}))
        avg-amp (avg (:amps analysis))
        avg-freq? (boolean (seq (:freqs analysis)))
        avg-freq (when avg-freq? (avg (:freqs analysis)))
        amp-norm-mult (normalize-amp (:max-amp analysis))]
    (swap! bufs update buf-key
           assoc
           :rec/time sample-start
           :analysis (-> analysis
                         (assoc :avg-amp avg-amp)
                         (assoc :avg-freq? avg-freq?)
                         (cond-> avg-freq? (assoc :avg-freq avg-freq))
                         (dissoc :amps :freqs))
           :amp-norm-mult amp-norm-mult)))

(defn norm-amp
  [buf]
  (:amp-norm-mult buf 1))

(defn silence?
  ([buf] (silence? 0.05 buf))
  ([threshold buf]
   (if-let [max-amp (-> buf :analysis :max-amp)]
     (do
       #_(timbre/info "silence? amp" max-amp)
       (< max-amp threshold))
     (do (timbre/warn "Buffer has no `:analysis :max-amp` key. Assuming it is silent.")
         true))))

(defn add-meta
  [buf-key section subsection input-name]
  (swap! bufs update buf-key
         assoc :rec/meta {:section section
                          :subsection subsection
                          :input-name input-name}))

(defn perform-query [query-map value-map]
  (->> query-map
       (map
        (fn [[k v]]
          (if (fn? v)
            (boolean (v (k value-map)))
            (= v (k value-map)))))
       (every? true?)))

(defn filter-by-rec-meta
  "Gets buffers that have a `:rec/meta` key and whose
  values equal the `rec-meta-query` map."
  [bufs rec-meta-query]
  (filter
   (fn [[_k buf]]
     (let [ks (keys rec-meta-query)
           meta-map (-> buf :rec/meta (select-keys ks))]

       (perform-query rec-meta-query meta-map)))
   bufs))

(comment
  ;; test
  (def test-derefed-bufs
    {:buf-1 {:rec/meta {:a :b :x :y}}
     :buf-2 {:rec/meta {:a :c :x :y}}
     :buf-3 {:rec/meta {:a :b :x :z}}})
  (filter-by-rec-meta test-derefed-bufs {:a :b})
  (filter-by-rec-meta test-derefed-bufs {:a #(= % :b)}))

#_:clj-kondo/ignore
(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus
                                              mic-4-bus
                                              preouts]])
  (reset! analysis-history {})
  (o/stop)
  (->> @analysis-history
       :mic-1-bus
       (map :amp)
       sort
       reverse)
  (-> mic-1-bus)
  (swap! recording? assoc :mic-1-bus true) ;; manually turn on run-receive-analysis
  (swap! recording? assoc :mic-1-bus false)
  (def receiver-analyzer (start-signal-analyzer :input-bus mic-1-bus))
  (o/kill (:analyzer receiver-analyzer)))

(def test-samples (atom {}))

(def load-own-samples! #'rec/load-own-samples!)
(def delete-sample! #'rec/delete-sample!)

(comment
  (rec/load-own-samples!
   :buffers-atom test-samples
   :prefixes-set #{:habitat/test-samples-v1}
   :samples-path habitat-samples-path))

(comment
  #_:clj-kondo/ignore
  (require '[tieminos.utils :refer [rrange]])
  (let [buf-key :amanecer-subsection-guitar-1]
    (o/demo
     #_(o/sin-osc)
     (* (norm-amp (-> @bufs buf-key))
        (o/play-buf 1 (-> @bufs buf-key)))))
  (-> guitar-bus)
  (o/demo (o/in guitar-bus))
  (normalize-amp 0.5)
  (-> @bufs)
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "guitar"
              :input-bus guitar-bus
              :dur-s (rrange 7 15)})
  (rec-input {:section "amanecer"
              :subsection "ide"
              :input-name "mic-1"
              :input-bus mic-1-bus
              :dur-s (rrange 7 15)}))

(defn rand-queried-buf [rec-query]
  (try (-> @bufs
           (filter-by-rec-meta rec-query)
           rand-nth
           second)
       (catch Exception e
         (timbre/warn "rand-queried-buf"
                      [(:section rec-query)
                       (:subsection rec-query)]
                      e))))

(defn weigthed-rand-queried-buf
  [{:keys [rec-query
           recent-amount
           recent-weight
           old-weight]}]
  (try
    (let [query-res (-> @bufs (filter-by-rec-meta rec-query))
          recency-k (weighted {:recent recent-weight
                               :old old-weight})
          bufs (case recency-k
                 :old query-res
                 :recent (->> query-res
                              (sort-by (comp :rec/time second))
                              reverse
                              (take recent-amount)))]
      (->> bufs rand-nth second))
    (catch Exception e
      (timbre/warn "weighted-rand-queried-buf"
                   [(:section rec-query)
                    (:subsection rec-query)]
                   e))))
