(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.save-synths
  "Saves selected audio buffers and synth params. Assumes synths that run on a `oe/defsynth`,
  that takes a single `:buf` key among other numerical params and may or may not contain a `:group`.

  Will use the habitat `db.edn` and will save the samples on the `/samples/habitat_samples` directory."
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :as rec]))

(def ^:private saved-synth-params-dir
    (str (System/getProperty "user.dir")
         "/src/tieminos/habitat/extended_sections/tunel_cuantico_bardo/saved_synth_params"))

(defn- serialize-group
  [synth-params]
  (let [group (-> synth-params :group second)]
    (if (-> (@groups/groups group))
      (assoc synth-params :group group)
      (do
        (timbre/warn "Serializing synth-params with unknown group.")
        (dissoc synth-params :group)))))

(defn- serialize-params*
  [{:keys [db-full-keyword buf->keys-map synth-params]}]
  (-> synth-params
      (update :buf (fn [b] [db-full-keyword (buf->keys-map b)]))
      serialize-group
      (dissoc :out)))


(defn- serialize-params
    "Serialize the params corresponding to the `params-indexes`."
  [{:keys [params-atom
           params-indexes
           db-full-keyword
           buffers-atom]}]
    (map #(serialize-params*
            {:db-full-keyword db-full-keyword
             :buf->keys-map (-> @buffers-atom
                                set/map-invert)
             :synth-params (nth @params-atom %)})
         params-indexes))


(defn- save-params*
    [file-name serialized-params]
    (spit (str saved-synth-params-dir "/" file-name)
          (str (into [] serialized-params))))


(defn save-params
  "Save the params to `file-name` under the `saved-synth-params-dir` and writes the used audio buffers to `/samples/habitat_samples/`.
  `file-name` should contain the .edn extension.
  `buffers-db-keyword-prefix` refers to the top-level keyword of the `/samples/habitat_samples/db.edn` file."
  [{:keys [params-file-name
           params-atom
           params-indexes
           buffers-db-keyword-prefix
           buffers-atom]}]
  (let [serialized-params (serialize-params {:params-atom params-atom
                                             :params-indexes params-indexes
                                             :db-full-keyword buffers-db-keyword-prefix
                                             :buffers-atom buffers-atom})]
    (rec/save-samples {:description (str "Buffers used by " params-file-name)
                       :full-keyword buffers-db-keyword-prefix
                       :buffers-atom (atom (select-keys @buffers-atom (map (comp second :buf) serialized-params)))})
    (timbre/info "`Audio buffers are being written.")
    (save-params* params-file-name serialized-params)
    (timbre/info "`synth-params have been saved.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recreate synth params with live buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-db-keyword-atom!
  [buffers-db-keyword-prefix]
  (rec/load-own-samples!
    :buffers-atom (atom {})
    :prefixes-set #{buffers-db-keyword-prefix}
    :samples-path rec/habitat-samples-path))

(defn rehydrate-synth-params
  "Recreate synth params with live buffers.
  Assumes `habitat/init!` has already been called as it requires it's groups to be initialized.
  `buffers-db-keyword-prefix` refers to the top-level keyword of the `/samples/habitat_samples/db.edn` file."
  [{:keys [buffers-db-keyword-prefix
           params-file-name
           groups
           default-group
           default-out]}]
  (let [buffers-atom (get-db-keyword-atom! buffers-db-keyword-prefix)]
    (->> (str saved-synth-params-dir "/" params-file-name)
         slurp
         edn/read-string
         (map (fn [params]
                (-> params
                    (update :buf (fn [[_ sample-key]]
                                   (if-let [buf (sample-key @buffers-atom)]
                                     buf
                                     (timbre/warn "Sample not found:"
                                                  {:sample-key sample-key
                                                   :synth-params params}))))
                    (update :group (fn [group]
                                     (if-let [group* (groups group)]
                                       group*
                                       default-group)))
                    ;; TODO out has not be serialized at the moment
                    (update :out (constantly default-out))))))))
