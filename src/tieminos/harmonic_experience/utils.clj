(ns tieminos.harmonic-experience.utils
  (:require
   [erv.scale.core :as scale :refer [interval->ratio]]
   [erv.utils.core :refer [interval]]))

(defn ratios->deg->ratio-map
  [sorted-ratios]
  (->> sorted-ratios
       (map-indexed (fn [i r] [r i]))
       (into {})))

(defn- midi->ratio*
  [ref-note sorted-ratios midi-note]
  (nth sorted-ratios
       (mod (- midi-note ref-note)
            (count sorted-ratios))))

(defn intervals [ratios]
  (->> ratios
       (sort)
       (partition 2 1)
       (map #(apply interval %))))

(defn- absolute-ratio
  [scale ref-note midi-note]
  (interval->ratio scale 0 (- midi-note ref-note)))

(def midi->ratio (memoize midi->ratio*))

(def ^:private scale->sorted-ratios
  (memoize
   (fn [scale]
     (map :bounded-ratio scale))))

(defn- midi->ratio&freq*
  [{:keys [ref-note root scale midi-note]}]
  (let [ratio  (midi->ratio ref-note (scale->sorted-ratios scale) midi-note)
        absolute-ratio* (absolute-ratio scale ref-note midi-note)]
    {:ratio ratio
     :absolute-ratio absolute-ratio*
     :freq (scale/deg->freq scale root (- midi-note ref-note))}))

(def midi->ratio&freq #'midi->ratio&freq*)
