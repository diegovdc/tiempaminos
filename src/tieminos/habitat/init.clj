(ns tieminos.habitat.init
  (:require
   [clojure.set :as set]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.core]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.habitat.panners :refer [circle-pan-4ch current-panners]]
   [tieminos.habitat.reactivity.amp :refer [init-amp-analyzer!]]
   [tieminos.habitat.recording :refer [start-signal-analyzer]]
   [tieminos.habitat.routing :as routing
    :refer [guitar-bus init-buses-and-input-vars! init-preouts!
            init-recordable-inputs! init-texto-sonoro-rand-mixer-synth! input inputs
            main-returns mic-1-bus mic-2-bus mic-3-bus mic-4-bus preouts recordable-outputs
            set-preout-in! special-inputs]]
   [tieminos.habitat.synths.main-fx :refer [init-main-fx!]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.utils :refer [init-async-seq-call-loop!]]))

(defonce inputs-registry (atom {}))
(defonce analyzers-registry (atom {}))

(defn- init-inputs! [inputs]
  (doseq [[_ {:keys [synth]}] @inputs-registry]
    (when (and synth (o/node-active? synth))
      (try (o/kill synth)
           (catch Exception _
             (timbre/warn "Synth does not exist anymore")))))
  (->> inputs
       (map (fn [[input-name {:keys [in bus]}]]
              {input-name {:out-bus bus
                           :synth (input {:group (groups/early)
                                          :in in
                                          :out bus})}}))
       (into {})
       (reset! inputs-registry)))

(defn- merge-analyzers-inputs
  [inputs recordable-outputs]
  (merge (->> inputs (map (fn [[k v]] [(keyword "recordable-input" (name k)) v]))
              (into {}))
         (->> recordable-outputs (map (fn [[k v]] [(keyword "recordable-output" (name k)) v]))
              (into {}))))

(defn- init-analyzers! [inputs recordable-outputs]
  (let [inputs* (merge-analyzers-inputs inputs recordable-outputs)]
    (doseq [[_ {:keys [analyzer]}] @analyzers-registry]
      (when (and analyzer (o/node-active? analyzer))
        (try (o/kill analyzer)
             (catch Exception _
               (timbre/warn "Signal analyzer does not exist anymore")))))
    (init-amp-analyzer!)
    (reset! analyzers-registry
            (into {}
                  (map
                   (fn [[input-name {:keys [in bus]}]]
                     {input-name (start-signal-analyzer :input-bus bus)})
                   inputs*)))))

(defonce habitat-initialized? (atom false))

(defn overtone-output-vol!
  [db]
  (timbre/info (format "Output volume set to: %sdb" db))
  (o/volume (o/db->amp db)))

(defn init!
  [& {:keys [_add-custom-groups-fn return-n-chans volume-db]
      :or {return-n-chans 4
           volume-db -12}
      :as init-config}]
  (when-not @habitat-initialized?
    (when (o/server-disconnected?)
      (tieminos.core/connect))
    (reaper/init)
    (habitat-osc/init (set/rename-keys init-config {:osc/port :port}))
    (init-async-seq-call-loop!)
    (reset! current-panners {})
    (reset! routing/return-n-chans return-n-chans)
    (groups/init-groups! init-config)
    (init-buses-and-input-vars!)
    (init-preouts! @inputs return-n-chans)
    (init-main-fx!) ;; TODO make octophonic versions
    (init-inputs! @inputs)
    (init-texto-sonoro-rand-mixer-synth! @special-inputs)
    (init-recordable-inputs! main-returns)
    (init-analyzers! @inputs @recordable-outputs)
    (overtone-output-vol! volume-db)
    (reset! habitat-initialized? true))
  (timbre/info (format "Habitat initialized! (%s channels)" return-n-chans))
  {:inputs (keys @inputs)})

(-> @preouts :guitar :bus)

(comment
  (o/stop)
  ;; TODO refactor initialization
  (init!)
  (-> @preouts :guitar :bus)
  (-> @inputs-registry :guitar :out-bus)
  (circle-pan-4ch
   {:group (groups/mid)
    :in guitar-bus
    :out (-> @preouts :guitar :bus)})
  (circle-pan-4ch {:group (groups/mid)
                   :in mic-1-bus
                   :out (-> @preouts :mic-1 :bus)})
  (circle-pan-4ch
   {:group (groups/mid)
    :in mic-2-bus
    :out (-> @preouts :mic-2 :bus)})
  (circle-pan-4ch
   {:group (groups/mid)
    :in mic-3-bus
    :out (-> @preouts :mic-3 :bus)})
  (circle-pan-4ch
   {:group (groups/mid)
    :in mic-4-bus
    :out (-> @preouts :mic-4 :bus)})
  (set-preout-in! :guitar guitar-bus))
