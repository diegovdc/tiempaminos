(ns tieminos.sc-utils.recording.v1
  (:require
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [overtone.core :as o]
   [overtone.helpers.audio-file :as oaf]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [tieminos.compositions.garden-earth.synths.granular :as gs]
   [tieminos.utils :refer [dur->bpm seconds->dur]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(defonce bufs (atom {}))
(defn alloc [bufs-atom key seconds]
  (let [buf (o/buffer (* seconds (o/server-sample-rate)))]
    (swap! bufs-atom assoc key buf)
    buf))

(o/defsynth writer
  [in-bus 0 buf 0 seconds 5]
  ;; `in-bus` should be a mono bus
  ;; "Use a custom input, with `o/in`"
  ;; https://depts.washington.edu/dxscdoc/Help/Classes/RecordBuf.html
  (let [in (o/in in-bus)
        env (o/env-gen (o/envelope [0 1 1 0]
                                   [0.01 (- seconds 0.02) 0.01]))]
    (o/record-buf:ar (* env in) buf :action o/FREE :loop 0)))

(defn rec-buf
  ([{:keys [bufs-atom buf-key seconds input-bus writer-fn]
     :or {writer-fn writer}}]
   (let [buf (alloc bufs-atom buf-key seconds)]
     (writer input-bus buf seconds)
     buf)))

(defn free-buffers []
  (doseq [b (vals @bufs)]
    (o/buffer-free b)))

(defn- name-buf-key [buf-key]
  (cond (some true? ((juxt list? vector? seq? set?) buf-key)) (str/join "-" (map name-buf-key buf-key))
        (int? buf-key) (str buf-key)
        (boolean? buf-key) (str buf-key)
        (keyword? buf-key) (name buf-key)
        :else (str buf-key)))

(defn make-progress-bar-fn
  [progress-range]
  (fn [index]
    (when (zero? index)
      (println progress-range))
    (print (case index
             0 "0"
             9 "2"
             10 "5"
             19 "5"
             20 "0"
             29 "7"
             30 "5"
             37 "1"
             38 "0"
             39 "0\n"
             "*"))
    (flush)))

(defn run-rec
  "NOTE: `input-bus` may be `nil`. This will use `o/sound-in` 0"
  [bufs-atom buf-key seconds input-bus & {:keys [progress-bar? print-info?]
                                          :or {print-info? true}}]

  (when print-info? (println (format "\nStarting!\n\n%s seconds" seconds)))
  (let [progress-range "0%                                  100%"
        durs (repeat 40 (/ 1 (count progress-range)))
        progress-bar-fn (when progress-bar?
                          (make-progress-bar-fn progress-range))]
    (when-not input-bus
      (throw (ex-info "No `input-bus` provided for recording." {})))
    (ref-rain :id (keyword "granular" (str "recording"
                                           (name-buf-key buf-key)))
              :tempo (dur->bpm (* seconds 1000))
              :loop? false
              :durs durs
              :on-event
              (on-event
               (when (zero? index)
                 (rec-buf {:bufs-atom bufs-atom
                           :buf-key buf-key
                           :seconds seconds
                           :input-bus input-bus}))
               (when progress-bar?
                 (progress-bar-fn index))))))

(def recording?
  "For external use only (an outside check if something is being recorded)"
  (atom false))

(defn start-recording
  "NOTE: `input-bus` may be `nil`. See `run-rec`."
  [& {:keys [bufs-atom buf-key seconds msg on-end countdown input-bus bpm _progress-bar? on-rec-start print-info?]
      :or {on-end (fn [_buf-key] nil)
           countdown 3
           bpm 60
           print-info? true}
      :as rec-config}]
  (ref-rain
   :id (keyword "recording" (str "start-recording"
                                 (name-buf-key buf-key)))
   :tempo bpm
   :loop? false
   :durs (conj (vec (repeat countdown (seconds->dur 1 bpm)))
               (seconds->dur seconds bpm)
               1)
   :on-event (on-event
              (do
                (when  (zero? index)
                  (timbre/info (str msg ": " buf-key))
                  (when print-info? (println "Countdown:")))

                (cond (> (- countdown index) 0)
                      (when print-info?
                        (print (str (- countdown index) "... "))
                        (flush))

                      (= countdown index)
                      (do (reset! recording? true)
                          (run-rec bufs-atom buf-key seconds input-bus rec-config)
                          (when on-rec-start
                            (on-rec-start rec-config)))

                      :else
                      (do (when print-info? (println "Done!"))
                          (reset! recording? false)
                          (on-end buf-key)))))))

(def default-samples-path
  (str (System/getProperty "user.dir")
       "/samples/recorded_samples/"))

(defn save-samples*
  "`prefix` is a unique identifier for the sample set"
  [prefix path buffers-atom name-buf-key-fn preserve-keys]
  (let [date (java.util.Date.)
        bufs* @buffers-atom
        db (atom (edn/read-string (slurp (str path "db.edn"))))]
    (timbre/info (format "Writing %s buffers to %s with prefix %s" (count bufs*) path prefix))
    (a/go
      (doseq [[k b] bufs*]
        (let [filename (str (name prefix) "-" (name-buf-key-fn k))
              path* (str path filename ".wav")]
          (timbre/info "Writing" filename)
          (oaf/write-audio-file-from-seq (o/buffer-read b)
                                         path*
                                         (:rate b)
                                         (:n-channels b))
          (swap! db assoc-in
                 [prefix k]
                 {:path path*
                  :id (:id b)
                  :key k
                  :date date
                  :prefix prefix
                  :buf-map-data (if preserve-keys (select-keys b preserve-keys) {})})))
      (spit (str path "db.edn") @db)
      (timbre/info "All done!"))))

(defn save-samples
  "`preserve-keys` additional keys in the buffer map that should be saved to the db"
  [prefix
   & {:keys [path buffers-atom name-buf-key-fn preserve-keys]
      :or {path default-samples-path
           buffers-atom bufs
           name-buf-key-fn name-buf-key
           preserve-keys []}}]
  (save-samples* prefix path buffers-atom name-buf-key-fn preserve-keys))

(comment
  (save-samples :test))

(defn load-own-samples!
  "`buffers-atom` is an atom like {k overtone.buffer}
  `dissoc-prefixes` a set of prefixes to dissoc"
  [& {:keys [buffers-atom prefixes-set samples-path dissoc-prefixes]
      :or {buffers-atom (atom {})
           prefixes-set #{}
           samples-path default-samples-path
           dissoc-prefixes #{:test}}}]
  (let [db (edn/read-string (slurp (str samples-path "db.edn")))
        files (->> (apply dissoc db dissoc-prefixes)
                   (filter (fn [[k]] (or (empty? prefixes-set) (prefixes-set k))))
                   (mapcat (comp seq second))
                   shuffle
                   (into {})
                   (map (fn [[_ data]]
                          [(:key data) (with-meta (merge (o/load-sample (:path data))
                                                         (:buf-map-data data {}))
                                         data)]))
                   (into {}))]
    (reset! buffers-atom (with-meta files {:samples-path samples-path}))
    buffers-atom))

(defn delete-sample!
  "`samples-atom` must have been created with `load-own-samples!` as it
  needs the `:samples-path` to be set in the metadata of the contained map."
  [samples-atom sample-key]
  (let [samples (deref samples-atom)
        samples-path (-> samples meta :samples-path)
        sample (get samples sample-key)
        db-path (str samples-path "db.edn")
        db (edn/read-string (slurp db-path))]
    (when sample
      (let [file-path (:path (meta sample))
            updated-db (update db (:prefix (meta sample))
                               dissoc (:key (meta sample)))
            updated-samples (with-meta (dissoc samples sample-key)
                              {:samples-path samples-path})]
        (try (io/delete-file file-path)
             (catch Exception _
               (timbre/error (str "Couldn't delete, file not found: "
                                  file-path))))
        (spit db-path updated-db)
        (reset! samples-atom updated-samples)))
    :done))

(defn filter*
  "Filters bufs by passing the `bufkey` to `f`"
  [f bufs]
  (filter (fn [[bufkey]] (f bufkey)) bufs))

(comment (filter* (fn [[_ id]] (= :oceanic id)) @bufs))

(comment

  (->> @bufs keys)
  (reset! bufs {})
  (def test-samples (load-own-samples! :prefixes-set #{:test}
                                       :dissoc-prefixes #{}))

  (->> test-samples)
  (->> prev-samples deref first second meta))

(comment
  (do
    (o/defsynth play-buf*
      [buf 0]
      (o/out 0 (o/play-buf 1 buf)))
    (defn replay [buf-key & {:keys [synth speed]
                             :or {synth (rand-nth [gs/sample&holdo
                                                   gs/ocean
                                                   gs/lines])
                                  speed 1}}]
      (when-let [b (@bufs buf-key)]
        (synth
         b
         (/ (:duration b) speed)
         :a 5
         :r 5
         :speed speed
         :trig-rate 40
         :grain-dur 1/20
         :rate (rand-nth [1 3 5 7
                          1/2 2/3
                           ;; 4/5 5/7
                          ])
         :amp (+ 0.3 (rand 2))
         :amp-lfo (* 3 (rand)))))

    (->> @bufs keys)

    (replay #_(->> @bufs keys rand-nth)
     :guitar-bus-1670702407392
            ;; :guitar-bus-1670701982972
            ;; :guitar-bus-1670701991874
            ;; :guitar-bus-1670702000967
            :synth gs/sample&hold
            :speed (+ 0.1 (rand))))
  (play-buf* (->> @bufs :guitar-bus-1670702407392))
  (->> @bufs keys)
  (save-samples :test)
  (o/stop)
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus]])
  (defn rec-input
    ;; FIXME make something easier to test without needing habitat routing
    ;; NOTE uses habitat routing
    [input-bus]
    (let [name* (:name input-bus)]
      (start-recording
       :bufs-atom bufs
       :buf-key (keyword name* (str  "-" (o/now)))
       :input-bus input-bus
       :seconds 5
       :msg "Test recording"
       :on-end replay)))

  (rec-input guitar-bus)
  (rec-input mic-2-bus))
