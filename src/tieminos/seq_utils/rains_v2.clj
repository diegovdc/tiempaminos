(ns tieminos.seq-utils.rains-v2
  "Helpers and subsequencers for use with time-time.dynacan.players.refrain.v2"
  (:require
   [time-time.dynacan.players.refrain.v2
    :as
    rain.v2
    :refer
    [on-event ref-rain]]))

(defn subrain
  [{:keys [ref ratio delay durs on-event config]}]
  (ref-rain
   (cond->
    {:id (random-uuid)
     :ref ref
     :durs (if-not delay durs (concat [delay] durs))
     :on-event on-event
     :loop? false}
     ratio (assoc :ratio ratio)
     config (merge config))))
