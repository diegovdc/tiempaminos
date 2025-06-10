(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.utils
  (:require
   [time-time.dynacan.players.refrain.v2
    :as
    rain.v2
    :refer
    [on-event ref-rain]]))

(defn subrain
  [{:keys [ref ratio delay durs on-event]}]
  (ref-rain
   (cond->
    {:id (random-uuid)
     :ref ref
     :durs (if-not delay durs (concat [delay] durs))
     :on-event on-event
     :loop? false}
     ratio (assoc :ratio ratio))))
