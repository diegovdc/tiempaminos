(ns tieminos.polydori.lattice
  (:require
   [erv.utils.core :refer [period-reduce]]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.polydori.scale :refer [polydori-v2]]))

(comment
  (def played-ratios (atom []))
  (defn on-note-on [{:keys [ratio absolute-ratio]}]
    (swap! played-ratios conj ratio))
  (-> @played-ratios
      frequencies)
  (hexp.lattice/setup-kb
   {:ref-note 31
    :root 1
    :scale (->> polydori-v2 :scale (map #(update % :bounded-ratio (fn [x] (period-reduce (/ x (* 64/45)))))))
    :midi-kb (overtone.midi/midi-in "Bus 2") #_(midi/get-lumatone!)
    :stroke-width 5
    :note-color [20 200 255]
    :sound? false
    :on-note-on #'on-note-on}))

(comment
  (map erv.utils.conversions/ratio->cents [1 7/6 105/76 3/2 135/76])
  (map erv.utils.conversions/ratio->cents [63/38])
  (->> [1 7/6 105/76 3/2 135/76]
       (map erv.utils.conversions/ratio->cents)
       (partition 2 1)
       (map (fn [[a b]] (- b a))))

  (erv.scale.core/print-scale-intervals!
   (erv.utils.ratios/ratios->scale
    [1 7/6 105/76 3/2 135/76])
   #_{:unit :ratios}))
