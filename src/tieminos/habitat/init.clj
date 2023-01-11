(ns tieminos.habitat.init
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.habitat.panners :refer [circle-pan-4ch current-panners]]
   [tieminos.habitat.recording :refer [start-signal-analyzer]]
   [tieminos.habitat.routing
    :refer [guitar-bus
            init-preouts!
            input
            inputs
            mic-1-bus
            mic-2-bus
            mic-3-bus
            mic-4-bus
            preouts
            set-preout-in!]]
   [tieminos.sc-utils.groups.v1 :as groups]))

(defonce inputs-registry (atom {}))
(defonce analyzers-registry (atom {}))

(defn- init-inputs! [inputs]
  (doseq [[_ {:keys [synth]}] @inputs-registry]
    (when (and synth (o/node-active? synth))
      (try (o/kill synth)
           (catch Exception _
             (timbre/warn "Synth does not exist anymore")))))
  (reset! inputs-registry
          (into {}
                (map
                 (fn [[input-name {:keys [in bus]}]]
                   {input-name {:out-bus bus
                                :synth (input {:group (groups/early)
                                               :in in
                                               :out bus})}})
                 inputs))))

(defn- init-analyzers! [inputs]
  (doseq [[_ {:keys [analyzer]}] @analyzers-registry]
    (when (and analyzer (o/node-active? analyzer))
      (try (o/kill analyzer)
           (catch Exception _
             (timbre/warn "Signal analyzer does not exist anymore")))))

  (reset! analyzers-registry
          (into {}
                (map
                 (fn [[input-name {:keys [in bus]}]]
                   {input-name (start-signal-analyzer :input-bus bus)})
                 inputs))))

(defn init! []
  (habitat-osc/init)
  (reset! current-panners {})
  (groups/init-groups!)
  (init-preouts! inputs)
  (init-analyzers! inputs)
  (init-inputs! inputs)
  {:inputs (keys inputs)})

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
