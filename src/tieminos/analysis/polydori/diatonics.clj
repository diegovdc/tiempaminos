(ns tieminos.analysis.polydori.diatonics
  (:require
   [clojure.set :as set]
   [tieminos.piraran.analysis :refer [dorian-hexanies-in-polydori]]
   [tieminos.piraran.scale :refer [polydori-deg->sets polydori-v2]]))


polydori-v2
polydori-deg->sets

(def dorian-hex-connections
  (->>
    (for [source dorian-hexanies-in-polydori
          target dorian-hexanies-in-polydori]
      (let [intersection (set/intersection (set (:degrees source))
                                           (set (:degrees target)))]
        (cond
          (= source target) nil
          (zero? (count intersection)) nil
          :else {(:name source) {:name (:name target)
                                 :degrees intersection
                                 :size (count intersection)}}
          )))
    (remove nil?)
    (reduce (fn [acc m]
              (update acc (ffirst m)
                      (comp #(sort-by :size > %) conj)
                      (first (vals m))))
            {})))
