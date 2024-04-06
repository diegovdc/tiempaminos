(ns tieminos.habitat.amp-trigger
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.overtone-extensions :as oe]))

(defonce handlers (atom {}))

(defn make-trigger-handler
  [handlers handler-args]
  (o/on-event "/tr"
              (fn [data]
                (when-let [{:keys [handler in] :as config} (get @handlers (first (:args data)))]
                  (handler {:event/data data
                            :config config
                            :in in
                            :args handler-args})))
              ::trigger-handler))

(oe/defsynth amp-trigger
  [in 0
   thresh 0.1]
  (let [sig (o/in in)
        amp (o/lag2 (o/amplitude sig 0.01 0.2) 0.1)
        trig (> amp thresh)]
    (o/send-trig:ar trig 0 amp)))

(defn reg-amp-trigger
  [{:keys [group in thresh handler handler-args]
    :or {in 0
         thresh 0.1}
    :as config}]
  (when-not handler
    (throw (ex-info "A `:handler` is required" {:config config})))

  (timbre/info "reg-amp-trigger thresh:" thresh)
  (make-trigger-handler handlers handler-args)

  (let [synth (amp-trigger (cond-> {:in in :thresh thresh}
                             group (assoc :group group)))]
    (swap! handlers assoc (:id synth) config)
    synth))

(defn dereg-handler
  [synth]
  (when-let [id (get @handlers (:id synth))]
    (swap! handlers dissoc id)
    (o/kill synth)))

(comment
  (init!)

  (o/stop)

  (def t1  (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                             :handler (fn [args] (println "hola" args))}))
  (def t2 (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                            :handler (fn [args] (println "adios" args))}))

  (dereg-handler t2)

  (osc/osc-debug false))
