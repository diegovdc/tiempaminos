(ns tieminos.polydori.analysis.cps-explorer
  (:require
   [clojure.string :as str]
   [erv.constant-structures.graphics :refer [init-cs-tool! update-state]]
   [erv.utils.conversions :as convo]
   [overtone.midi :as midi]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.polydori.scale :refer [polydori-set->deg polydori-v2]]
   [tieminos.utils :refer [map-subscale-degs]]))

;; TODO move this a polydori

(def sink (midi/midi-out "VirMIDI"))

(comment

  (->> polydori-v2 :scale
       (map :bounded-ratio)
       (map (juxt identity convo/ratio->cents)))

  (def dekany-keys
    (->> polydori-v2 :subcps
         (filter #(or (str/includes? (first %) "2)5")
                      (str/includes? (first %) "3)5")))
         sort
         keys))

  (def current-key-index (atom 0))
  (def cs-tool (init-cs-tool! [] []))
  (swap! cs-tool assoc :max-arcs 200)
  (def polydori-ratio->degree
    (reduce
     (fn [m {:keys [bounded-ratio degree]}]
       (assoc m bounded-ratio degree))
     {}
     (:scale polydori-v2)))

  (swap! current-key-index inc)         ;; TODO left at 14

  (let [dekany-key (nth dekany-keys @current-key-index)
        scale (-> polydori-v2 :subcps
                  (get dekany-key)
                  :scale)
        scale-degrees (->> scale (map :set)
                           (map polydori-set->deg)
                           set
                           sort)
        added-notes [#_128/95
                     #_8/7
                     #_(convo/cents->ratio 254)
                     #_(convo/cents->ratio 954)
                     #_(convo/cents->ratio 920)
                     #_(convo/cents->ratio 190)
                     #_(convo/cents->ratio 300)]
        scale-degrees (->> scale-degrees
                           (remove #(#{15} %))
                           (concat (map polydori-ratio->degree added-notes))
                           sort)
        _ (println dekany-key "///" (count scale-degrees) scale-degrees)
        deg-fn (fn [deg]
                 (let [deg* (- (map-subscale-degs
                                29
                                scale-degrees
                                deg)
                               90)]
                   deg*))
        deg->set (fn [deg]
                   (-> polydori-v2 :scale
                       (nth (mod deg 29))))]
    (println (map :bounded-ratio scale))
    (update-state cs-tool (->> scale
                               (remove #(= 256/171 (:bounded-ratio %)))) added-notes)
    (midi-in-event
     :note-on (fn [{:keys [note velocity] :as ev}]
                (let [note* (deg-fn note)]
                  (println note* ((juxt :bounded-ratio :sets) (deg->set note*)))
                  (midi/midi-note-on sink note* velocity)))
     :note-off (fn [{:keys [note] :as ev}]
                 (let [note* (deg-fn note)]
                   (midi/midi-note-off sink note*))))))
