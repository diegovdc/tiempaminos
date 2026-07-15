(ns tieminos.scales.core
  "Namespace for exporting the whole scale directory"
  (:refer-clojure :exclude [get list])
  (:require
   [clojure.string :as str]
   [erv.scale.scl :as scl]
   [taoensso.timbre :as timbre]
   [tieminos.scales.17o7.core :as *17o7]
   [tieminos.scales.diffractions.meta-slendro :as diffracted-meta-slendro]
   [tieminos.scales.grady :as grady]))

(def scales
  {:17o7 *17o7/scales
   :diffracted/meta-slendro diffracted-meta-slendro/scales
   :grady grady/scales})

(def ^:private default-scl-dir "/Users/diego/Music/tunings/")

(defn get* [& path]
  (get-in scales path))

(defn get [& path]
  (:scale (apply get* path)))

(defn list
  ([& dir-exclusions]
   (->> (apply dissoc scales dir-exclusions)
        (map (fn [[dir ms]] [dir (keys ms)]))
        (sort-by first))))

(defn- spit-scl
  [paths-atom dir {:keys [meta] :as scale-data}]
  (if-not (:scl/name meta)
    (throw (ex-info "`:meta :scl/name` is required" scale-data))
    (let [path (str default-scl-dir dir "/" (:scl/name meta))]
      (when (@paths-atom path)
        (timbre/warn "This file is being overwritten, must be a unique name:" path))
      (swap! paths-atom conj path)
      (scl/spit-file path scale-data)
      (timbre/debug "SCL file written at:" path))))

(defn- make-dirname [dir-kw]
  (-> dir-kw
      str
      (str/replace #":" "")
      (str/replace #"/" "_")))

(defn- update-tieminos-scl-archive
  [scales]
  (let [paths-atom (atom #{})]
    (timbre/info "Updating tieminos-archive...")
    (doseq [[dir scales*] scales]
      (let [dir* (str "tieminos-archive/" (make-dirname dir))]
        (doseq [[_k scale] scales*]
          (spit-scl paths-atom dir* scale))))
    (timbre/info "Done!")))

(comment
  (update-tieminos-scl-archive scales))
