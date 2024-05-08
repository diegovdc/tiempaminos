(ns tieminos.harmonic-experience.utils
  (:require
   [erv.scale.core :as scale]))

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

(def midi->ratio (memoize midi->ratio*))

(def ^:private scale->sorted-ratios
  (memoize
    (fn [scale]
      (map :bounded-ratio scale))))

(defn- midi->ratio&freq*
  [{:keys [ref-note root scale midi-note]}]
  {:ratio (midi->ratio ref-note (scale->sorted-ratios scale) midi-note)
   :freq (scale/deg->freq scale root (- midi-note ref-note))}
  )

(def midi->ratio&freq #'midi->ratio&freq*)
