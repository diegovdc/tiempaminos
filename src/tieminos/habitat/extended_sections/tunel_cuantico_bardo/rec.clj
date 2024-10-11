(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec
  (:require
   [tieminos.habitat.recording :refer [rec-input]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(defn start-rec-loop!
  [{:keys [id
           input-bus
           rec-dur-fn
           rec-pulse
           countdown
           on-rec-start]
    :or {id :rec-loop3
         rec-dur-fn (fn [_] 0.5)
         rec-pulse [0.5]
         countdown 0
         on-rec-start (fn [_])}}]
  (ref-rain
    :id id
    :durs rec-pulse
    :on-event (on-event
                (let [dur-s (-> (rec-dur-fn {:index index})
                                (- 0.05)
                                (max 0.01))]
                  (rec-input {:section "gusano-cuantico-bardo"
                              :subsection "default-subsection"
                              :input-name (:name input-bus)
                              :input-bus input-bus
                              :dur-s dur-s
                              :on-end (fn [_])
                              :print-info? false
                              :countdown countdown
                              :on-rec-start on-rec-start})))))
