(ns tieminos.web-visualizer.refrain-utils-v0
  "Provides an interface and helpers focused on `ref-rain` to generate and send data to ther web events-visualizer."
  (:require
   [clojure.edn :as edn]
   [erv.cps.core :as cps]
   [erv.scale.core :refer [deg->freq]]
   [erv.utils.conversions :refer [cps->midi cps->name]]
   [org.httpkit.client :as http]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.refrains :as hunu-refrains]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(comment
  ;; Usage
  (declare get-visualizer-data event->viz-event send-events!
           ;; NOTE helper functions
           adr-dur grain-sample-freq)

  (def scale (:scale (cps/make 2 [1 3 5 7])))
  (def events (atom []))

  (defn test-refrain
    [{:keys [refrain-ratio ;; used for sped up generation
             on-play] ;; used to save data into the visualizer
      :or {refrain-ratio 1}}]
    (ref-rain
     :id :test
     :durs [1]
     :ratio refrain-ratio
     :on-event (on-event
                (on-play {:refrain/data data
                          :meta {:instrument :guitar}
                          :synth-args {:freq (deg->freq scale 200 (at-i (range 20)))}}))))

  ;; Generate the events
  ;; NOTE `refrain-playback-rate` is used to accelarate the playback of the refrain so that the events can be generated quickly
  (test-refrain
   (let [refrain-playback-rate 2
         end-after-ms 2000
         refrain-ratio 1/2]
     {:refrain-ratio (/ refrain-ratio refrain-playback-rate)
      :on-play (fn [event-data]
                 (let [viz-data (get-visualizer-data event-data
                                                     {:refrain-playback-rate refrain-playback-rate
                                                       ;; NOTE helper functions can be used here, i.e. adr-dur grain-sample-freq, see arity of function
                                                      })]
                   (swap! events conj viz-data)
                   (when (>= (:elapsed-ms viz-data) end-after-ms)
                     (println "Ended")
                     (gp/stop (:refrain/id viz-data)))))}))

  ;; after events have been generated
  (->> @events (map event->viz-event) send-events!)

  (gp/stop)
  :rcf)

(defn- freq->px
  ([freq] (freq->px 16 freq))
  ([scaling-factor freq]
   (int (* scaling-factor (cps->midi freq)))))

(defn- ms->px
  [ms]
  (int (/ ms 100)))

(defn grain-sample-freq
  ([event-data] (grain-sample-freq nil event-data))
  ([default-root-freq {:keys [synth-args] :as _event-data}]
   (let [freq (-> synth-args :buf :analysis :avg-freq)
         rate (:rate synth-args)]

     (cond
       (and freq rate) {:freq (* freq rate)
                        :freq-type :avg}
       (and default-root-freq rate) {:freq (* freq rate)
                                     :freq-type :inharmonic}
       :else nil))))

(defn adr-dur
  [{:keys [synth-args] :as _event-data}]
  (let [{:keys [a d r]} synth-args]
    (when (and a d r) (* 1000 (+ a d r)))))

(defn- get-freq
  [{:keys [synth-args] :as event-data}
   default-root-freq freq-fn]
  (let [freq-val (try (freq-fn event-data)
                      (catch Exception _ nil))]
    (cond
      (:freq synth-args) {:freq (:freq synth-args)
                          :freq-type :exact}
      freq-val freq-val
      :else {:freq default-root-freq
             :freq-type :default})))

(defn get-visualizer-data*
  "From `event-data`, generate visualizer data.
  See keys of `event-data`.
  `refrain/data` is the `data` var (hashmap) `on-event` provides."
  [{:as event-data
    :keys [refrain/data synth-args meta]}
   {:as _config
    :keys [refrain-playback-rate default-root-freq freq-fn dur-fn]
    :or {refrain-playback-rate 1
         default-root-freq 100
         dur-fn (fn [_event-data] nil) ;; should return value in ms
         freq-fn (fn [_event-data] nil)}}]
  (let [{:keys [freq freq-type]} (get-freq event-data default-root-freq freq-fn)
        dur-ms (or (dur-fn event-data)
                   (* 1000 (:dur synth-args))
                   (-> data :dur-ms (* refrain-playback-rate)))]
    {:refrain/id (-> data :refrain/config :id)
     :refrain-ratio  (-> data :ratio (* refrain-playback-rate))
     :dur-ms  dur-ms
     :elapsed-ms (-> data :elapsed-ms (* refrain-playback-rate))
     :freq freq
     :meta (merge meta {:synth-args synth-args
                        :freq freq
                        :dur-ms dur-ms
                        :freq-type freq-type})}))

(defn get-visualizer-data-dispatch-fn
  [event-data]
  (-> event-data :meta :context/web-visualizer-fn-id))

(defmulti get-visualizer-data #'get-visualizer-data-dispatch-fn)

(defmethod get-visualizer-data nil
  [event-data]
  (throw (ex-info "Don't know how to visualize data for"
                  {:event-data event-data})))

(defmethod get-visualizer-data
  ::hunu-refrains/hacia-un-nuevo-universo-1
  [{:keys [meta] :as event-data}]
  (get-visualizer-data* event-data
                        {:refrain-playback-rate (:context/refrain-playback-rate meta)
                         :freq-fn (partial grain-sample-freq (:context/refrain-playback-rate meta 112))
                         :dur-fn adr-dur}))

(defn event->viz-event
  "Converts an event generated by `get-visualizer-data` into an event that can be sent to the web visualizer."
  [{:keys [refrain/id dur-ms elapsed-ms freq meta]
    :as _visualizer-data-event}]
  {:label (str id)
   :width (ms->px dur-ms)
   :x (ms->px elapsed-ms)
   :y (freq->px freq)
   :color "lightgreen"
   :meta meta
   })

(def ^:private c-names
  [[1.0 "C-5"]
   [2.0 "C-4"]
   [4.0 "C-3"]
   [8.0 "C-2"]
   [16.0 "C-1"]
   [32.0 "C0"]
   [64.0 "C0"]
   [128.0 "C1"]
   [256.0 "C2"]
   [512.0 "C3"]
   [1024.0 "C4"]
   [2048.0 "C5"]
   [4096.0 "C6"]
   [8192.0 "C7"]
   [16384.0 "C8"]
   [32768.0 "C9"]])

(do
  (defn add-c-lines
    [events]
    (if-not (seq events)
      []
      (let [freqs (map #(-> % :meta :freq) events)
            min* (apply min freqs)
            max* (apply max freqs)
            bounding-grid (->> c-names
                               (take-while #(<= (first %) max*))
                               (drop-while #(<= (* 2 (first %)) min*)))]

        (map (fn [[freq name*]]
               {:y (freq->px freq) ;; invert the order of the freq axis, because on browsers the y axis starts on the top, but usual freq representation starts at the bottom
                :label (format "%s (%shz)" name* (int freq))})
             bounding-grid)))))

(comment
  (cps->name 1)
  (->> @tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.nuebulosa-web-visualizer-test/events
       (map #(get-visualizer-data %))
       (map event->viz-event)
       add-c-lines))

(comment
  ;; TODO left here
  ;;
  ;; https://sneakycode.net/custom-edn-readers-for-dummies
  ;;
  (defn reverse-reader [coll]
    (assert (coll? coll) "you can only reverse collections")
    (reverse coll))

  (def custom-readers {'function str})

  (def some-edn "{:a 1
                :b [1 2 3 4 5]
                :c #function[clojure.core/min]}")

  (def result-data
    (clojure.edn/read-string {:readers custom-readers} some-edn))

  (:c result-data) ;

  (println min)

  (->> @tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.nuebulosa-web-visualizer-test/events
       (spit "src/tieminos/web_visualizer/sample-events.edn")
       #_(map #(get-visualizer-data %))
       #_(map event->viz-event)
       #_get-freq-boundaries)

  (edn/read-string {:readers {'function str}} (slurp "src/tieminos/web_visualizer/sample-events.edn"))

  (defn custom-reader [tag val]
    (if (= tag :function)
      (str "Function tag: " val)
      val))

  (defn read-edn [s]
    (let [options {:default (fn [tag val] (str "Unknown tag: " tag val))
                   :readers {'function custom-reader}}]
      (edn/read-string options s)))

  ;; Example usage
  (def edn-string "#function[time-time.dynacan.players.gen-poly/ref-rain/fn--27197]")

  (def result (read-edn edn-string))
  (println result))

(defn collect-event!
  [events-atom {:keys [event context root-freq]}]
  (timbre/debug "Collecting event")
  (let [playback-rate (:sequencer/refrain-playback-rate context 1)]
    (swap! events-atom conj
           (-> event
               (assoc-in [:meta :context/refrain-playback-rate] playback-rate)
               (assoc-in [:meta :context/root-freq] root-freq)))))

(defn fast-event-rendering?
  [context]
  (not= 1 (:sequencer/refrain-playback-rate context 1)))

(defn send-events!
  "Sends a seq of `events` to the web visualizer.
  `events` is a seq of events as generated by `event->viz-event`"
  [events]
  (http/post "http://localhost:5000/visualizer-data"
             {:body (pr-str {:events (map #(dissoc % :meta) events)
                             :config {:px-per-second (ms->px 1000)
                                      :horizontal-grid-lines (add-c-lines events)}})}
             (fn [{:keys [status _headers _body error]}] ;; asynchronous response handling
               (if error
                 (println "Failed, exception is " error)
                 (println "Async HTTP POST: " status)))))
