(ns tieminos.compositions.garden-earth.synths.recording
  (:require
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [overtone.core :as o]
   [overtone.helpers.audio-file :as oaf]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base
    :refer [bpm
            dur->bpm
            eik-notes
            on-event
            pitch-class->pr-fingering
            ref-rain
            seconds->dur
            stop]]
   [tieminos.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [tieminos.compositions.garden-earth.synths.granular :refer [grain]]))

(defonce bufs (atom {}))
(defn alloc [bufs-atom key seconds]
  (let [buf (o/buffer (* seconds (o/server-sample-rate)))]
    (swap! bufs-atom assoc key buf)
    buf))


(o/defsynth writer [buf 0 seconds 5]
  ;; https://depts.washington.edu/dxscdoc/Help/Classes/RecordBuf.html
  (let [in (o/free-verb (o/sound-in 0) 1)
        env (o/env-gen (o/envelope [0 1 1 0]
                                   [0.01 (- seconds 0.02) 0.01]))]
    (o/record-buf:ar (* env in) buf :action o/FREE :loop 0)))

(o/defsynth writer2
  "Use a custom input"
  [in 0 buf 0 seconds 5]
  ;; https://depts.washington.edu/dxscdoc/Help/Classes/RecordBuf.html
  (let [in (o/in 0)
        env (o/env-gen (o/envelope [0 1 1 0]
                                   [0.01 (- seconds 0.02) 0.01]))]
    (o/record-buf:ar (* env in) buf :action o/FREE :loop 0)))

(defn rec-buf
  ([bufs-atom buf-key seconds] (rec-buf bufs-atom buf-key seconds nil))
  ([bufs-atom buf-key seconds in]
   (let [buf (alloc bufs-atom buf-key seconds)]
     (if in
       (writer2 in buf seconds)
       (writer buf seconds))
     buf)))

(defn free-buffers []
  (doseq [b (vals @bufs)]
    (o/buffer-free b)))

(defn- name-buf-key [buf-key]
  (cond (some true? ((juxt list? vector? seq? set?) buf-key))
        (str/join "-" (map name-buf-key buf-key))
        (int? buf-key) (str buf-key)
        (boolean? buf-key) (str buf-key)
        :else (name buf-key)))

(comment (name-buf-key :oli))

(defn run-rec
  "NOTE: `input-bus` may be `nil`. This will use `o/sound-in` 0"
  [bufs-atom buf-key seconds input-bus]
  (println (format "Starting!\n\n%s seconds" seconds))
  (let [progress-range "0%                                  100%"
        durs (repeat 40 (/ 1 (count progress-range)))]
    (ref-rain :id (keyword "granular" (str "recording"
                                           (name-buf-key buf-key)))
              :tempo (dur->bpm (* seconds 1000))
              :loop? false
              :durs durs
              :on-event
              (on-event
               (cond (zero? index)
                     (do (println progress-range)
                         (print "|")
                         (if input-bus
                           (rec-buf bufs-atom buf-key seconds input-bus)
                           (rec-buf bufs-atom buf-key seconds)))

                     (= index (dec (count durs)))
                     (do (print "|") (println ""))

                     (< index (- (count progress-range) 10)) (print "=")
                     :else (print "*"))))))


(def recording?
  "For external use only (an outside check if something is being recorded)"
  (atom false))

(defn start-recording
  "NOTE: `input-bus` may be `nil`. See `run-rec`."
  [& {:keys [bufs-atom buf-key seconds msg on-end countdown input-bus]
      :or {on-end (fn [buf-key] nil)
           countdown 10}}]
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
                (when (zero? index)
                  (do (timbre/info (str msg " \n buf-key: " buf-key))
                      (println "Countdown:" )))

                (cond (> (- countdown index) 0)
                      (print (str (- countdown index) "... ") )

                      (= countdown index)
                      (do (reset! recording? true)
                          (run-rec bufs-atom buf-key seconds input-bus))

                      :else
                      (do (println "Done!")
                          (reset! recording? false)
                          (on-end buf-key)))))))

(defn replay [buf-key & {:keys [speed]
                         :or {speed 1}}]
  (let [b (@bufs buf-key)]
    (grain b
           (/ (:duration b) speed)
           :speed speed
           :trig-rate 40
           :grain-dur 1/20
           :pos-noise-amp 0
           :amp 3)))


(def default-samples-path
  "/home/diego/tiempaminos/erv-fib-synth/src/tieminos/compositions/garden_earth/samples/")

(defn save-samples*
  "`prefix` is a unique identifier for the sample set"
  [prefix path buffers-atom name-buf-key-fn]
  (let [date (java.util.Date.)
        bufs* @buffers-atom
        db (atom (edn/read-string (slurp (str path "db.edn"))))]
    (timbre/info (format "Writing %s buffers to %s" (count bufs*) path))
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
                 {:path path* :key k :date date :prefix prefix})))
      (spit (str path "db.edn") @db)
      (timbre/info "All done!"))))

(defn save-samples
  [prefix
   & {:keys [path buffers-atom name-buf-key-fn]
      :or {path default-samples-path
           buffers-atom bufs
           name-buf-key-fn name-buf-key}}]
  (save-samples* prefix path buffers-atom name-buf-key-fn))

(comment
  (save-samples :test-2))

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
                          [(:key data) (with-meta (o/load-sample (:path data))
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



(defn get-any-buffer-for-pitch-class
  ([pc] (get-any-buffer-for-pitch-class pc @bufs))
  ([pc bufs] (->> bufs
                  (filter (fn [[[k]]] (= k pc)))
                  rand-nth
                  second)))
(comment
  (get-any-buffer-for-pitch-class "A+53" @test-samples))



(defn rec-flute
  "Plays a reference pitch using `tuning-monitor`
  `buf-key` should be [pitch-class-string & ids] "
  [seconds buf-key on-end]
  (let [pitch-class (first buf-key)]
    (when (> 10 seconds)
      (tuning-monitor (-> eik-notes (get pitch-class) :bounded-ratio (* 440))
                      seconds
                      seconds))
    (tuning-monitor (-> eik-notes (get pitch-class) :bounded-ratio (* 440))
                    10
                    seconds)
    (start-recording
     :bufs-atom bufs
     :buf-key buf-key
     :seconds seconds
     :msg (format "\nWill record %s for %s seconds\nFingering:\n%s \n\n"
                  pitch-class
                  seconds
                  (pitch-class->pr-fingering pitch-class))
     :on-end on-end)))


(defn filter*
  "Filters bufs by passing the `bufkey` to `f`"
  [f bufs]
  (filter (fn [[bufkey]] (f bufkey)) bufs))

(comment (filter* (fn [[_ id]] (= :oceanic id)) @bufs))


(comment

  (->> bufs)
  (def test-samples (load-own-samples! :prefixes-set #{:test}
                                       :dissoc-prefixes #{}))

  (->> test-samples)
  (->> prev-samples deref first second meta))



(comment
  (rec-buf bufs [:oli 5 6] 5)
  (replay ["A+53" :a])
  (save-samples :test)
  (stop)
  (start-recording
   :bufs-atom bufs
   :buf-key :olips
   :seconds 5
   :msg "Will record G#5+45"
   :on-end replay))
