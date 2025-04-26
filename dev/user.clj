(ns user
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.tools.namespace.repl :as repl :refer [refresh set-refresh-dirs]]
   [erv.scale.scl :as scl]
   [overtone.core :as o]
   [overtone.sc.machinery.server.connection :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.osc.core :refer [osc-servers stop-server]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.scales.core :as scales]
   [time-time.dynacan.players.gen-poly :as gp]))

(timbre/set-level! :info)

(comment
  (repl/clear)
  ;; FIXME Ya casi funciona solo hay que arreglar el require en tieminos.compositions.garden-earth.synths.granular
  (refresh))

(set-refresh-dirs "src" "dev" "test")

(defn restart []
  (doseq [[port _] @osc-servers]
    (stop-server port))
  (repl/refresh))

#_:clj-kondo/ignore
(defn init-garden-earth! []
  (in-ns 'erv-fib-synth.compositions.garden-earth.core))

#_:clj-kondo/ignore
(def stop gp/stop)

(defn is-process-running? [process-name]
  (let [cmd-result (sh "pgrep" process-name)]
    (= 0 (:exit cmd-result))))

(defn connect []
  (if-not (is-process-running? "scsynth")
    (timbre/error "scscynth not running, please start it.")
    (o/connect-external-server)))

(defn test-sound []
  (o/demo (* 0.2 (o/pan2 (o/sin-osc))))
  :sounding...?)

(defn disconnect []
  (oc/shutdown-server)
  :disconnected)

(defn rec
  "
  Options:

  :n-chans     - Number of channels for the buffer
                 Default 2
  :size        - Buffer size
                 Default 65536
  :header      - Header format: \"aiff\", \"next\", \"wav\", \"ircam\", \"raw\"
                 Default \"wav\"
  :samples     - Sample format: \"int8\", \"int16\", \"int24\", \"int32\",
                                \"float\", \"double\", \"mulaw\", \"alaw\"
                 Default \"int16\"
  Example:  (rec \"prueba-4ch\" :n-chans 4)
  "
  [filename & opts]
  (apply o/recording-start
         (str (System/getProperty "user.dir")
              "/recordings/"
              filename
              "-"
              (str/replace (.format (java.time.ZonedDateTime/now)
                                    java.time.format.DateTimeFormatter/ISO_INSTANT)
                           #":"
                           "-")
              ".wav")
         opts))

(def rec-stop o/recording-stop)

(comment
  (connect)
  (disconnect)
  (timbre/set-level! :debug)
  (timbre/set-level! :info))

(def warn-on-debug
  (memoize
   (fn [ns-name]
     (timbre/warn "Trying to log a debug statement an external ns, but debugging external files is disabled. See timbre config.  ns: " ns-name))))

(defn print-debug?
  [level-name ns-name]
  (cond  (and (= level-name "debug")
              (str/includes?  ns-name "tieminos")) true
         (= level-name "debug") (warn-on-debug ns-name)
         :else true))

(timbre/merge-config!
 {:output-fn
  #_timbre/default-output-fn
  (fn [data]
    (let [{:keys [level ?err #_vargs msg_ ?ns-str ?file #_hostname_
                  timestamp_ ?line]} data]
      (when (print-debug? (name level) ?ns-str)
        (str
         (-> (force timestamp_) (str/split #" ") second) " "
         (str/upper-case (first (name level)))  " "
         "[" (or ?ns-str ?file "?") ":" (or ?line "?") "] - "
         (force msg_)
         (when-let [err ?err]
           (str "\n" (timbre/stacktrace err)))))))})

#_:clj-kondo/ignore
(defn ap-synth [synth-var arg-map]
  (let [ks (->> (meta synth-var) :arglists first (map keyword))
        synth (var-get synth-var)
        defaults (->> synth meta :params
                      (map (fn [{:keys [name default]}] [(keyword name) default]))
                      (into {}))
        vs (map #(get arg-map % (defaults %)) ks)]
    (apply synth vs)))

(defn test-4chan-surround
  "`sound` #{:saw :white}"
  [& {:keys [freq dur amp sound]
      :or {freq 200
           dur 10
           amp 0.2
           sound :white}}]
  ((o/synth []
            (let [f (mapv #(* % freq) [1 1.2 1.34 1.5])
                  in (if (= :saw sound)
                       (o/mix (o/saw f))
                       (o/white-noise))]
              (o/out 0 (* (o/env-gen (o/envelope [0 1 1 0]
                                                 (map #(* % dur) [0.2 0.6 0.2]))
                                     :action o/FREE)
                          (oe/circle-az :num-channels 4
                                        :in (* amp in)
                                        :pos (o/lf-saw 0.2)))))))
  :surrounding...?)

(def default-scl-dir "/Users/diego/Music/tunings/")

(defn spit-scl
  [{:keys [meta] :as scale-data}]
  (if-not (:scl/name meta)
    (throw (ex-info "`:meta :scl/name` is required" scale-data))
    (scl/spit-file (str default-scl-dir (:scl/name meta) ".scl") scale-data)))

(defn scales
  ([] (scales []))
  ([path]
   (if-not (seq path)
     (var-get #'scales/scales)
     (get-in (var-get #'scales/scales) path))))

(defn scales-keys
  [& {:keys [pprint?]}]
  (let [data (->> (scales)
                  (mapcat (fn [[k scales*]] (map (fn [scale-k] [k scale-k])
                                                 (sort (keys scales*)))))
                  (sort-by first))]
    (when pprint? (pprint data))
    data))

(comment
  (scales)
  (scales-keys)
  (connect)
  (test-sound)
  (test-4chan-surround)
  (disconnect))
