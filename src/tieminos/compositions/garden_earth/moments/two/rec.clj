(ns tieminos.compositions.garden-earth.moments.two.rec
  (:require
   [tieminos.habitat.recording :refer [rec-input]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

;; TODO update habitat.tunel-cuantic.rec with this and remove
(defn start-rec-loop!
  [{:keys [id
           input-bus
           rec-dur-fn
           rec-pulse
           countdown
           on-rec-start
           on-end
           section subsection]
    :or {id :ge.two/default-rec-loop
         rec-dur-fn (fn [_] 0.5)
         rec-pulse [0.5]
         countdown 0
         on-rec-start (fn [_])
         on-end (fn [_])
         section "in-volcanic-time"
         subsection "default-subsection"}}]
  (ref-rain
    :id id
    :durs rec-pulse
    :on-event (on-event
                (let [dur-s (-> (rec-dur-fn {:index index})
                                (- 0.05)
                                (max 0.01))]
                  (rec-input {:group (groups/output-rec)
                              :section section
                              :subsection subsection
                              :input-name (:name input-bus)
                              :input-bus input-bus
                              :dur-s dur-s
                              :on-end on-end
                              :print-info? false
                              :countdown countdown
                              :on-rec-start on-rec-start})))))
