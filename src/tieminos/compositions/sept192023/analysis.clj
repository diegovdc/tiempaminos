(ns tieminos.compositions.sept192023.analysis
  (:require
   [tieminos.harmonic-experience.lattice :refer [draw-lattice]]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer
    [dorian-hex-connections dorian-hexanies-in-polydori-by-name]]
   [tieminos.polydori.scale :refer [polydori-v2]]))

(def cps
  {:beating-pads "diat2v2"
   :bass "diat4v2"
   :randy-lights "diat6v3"})

(-> dorian-hex-connections
    (get "diat2v2"))

;; common tones
(let [all-cps (set (vals cps))]
  (map
   (fn [[k hex-name]]
     [k (filter #(all-cps (:name %)) (get dorian-hex-connections hex-name))])
   cps))

(def all-harmonies-data
  (let [used-degrees (-> dorian-hexanies-in-polydori-by-name
                         (select-keys (vals cps))
                         vals
                         (->> (mapcat :degrees))
                         set
                         sort)]
    {:size (count used-degrees)
     :used-degrees used-degrees
     :ratios (map (fn [deg] (->  polydori-v2 :scale (nth deg) :bounded-ratio))
                  used-degrees)}))

(comment
  (draw-lattice
   {:width 1200
    :ratios (->> all-harmonies-data
                 :ratios)
    :text-type :factors})
  :rcf)
