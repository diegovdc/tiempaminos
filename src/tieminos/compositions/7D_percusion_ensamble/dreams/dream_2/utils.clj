(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.utils
  (:require
   [time-time.dynacan.players.refrain.v2
    :as
    rain.v2
    :refer
    [on-event ref-rain]]))

(defn subrain
  [{:keys [ref ratio durs on-event]
    :or {ratio 1}}]
  (ref-rain
   :id (random-uuid)
   :ref ref
   :ratio ratio
   :durs durs
   :on-event on-event
   :loop? false))
