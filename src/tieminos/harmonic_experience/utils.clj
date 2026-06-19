(ns tieminos.harmonic-experience.utils
  (:require
   [erv.scale.core :as scale :refer [interval->ratio]]
   [erv.utils.core :refer [interval]]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.drones.sounds :refer [drone]]
   [tieminos.harmonic-experience.sustainer :as legato]
   [tieminos.utils :refer [wrap-at]]))

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

(defn drone-box
  ([root scale degrees] (drone-box root scale degrees [0.5]))
  ([root scale degrees amps]
   (let [config (->> degrees
                     (map-indexed (fn [i deg] (let [ratio (:bounded-ratio (wrap-at deg scale))]
                                                {deg {:inst drone :params {:freq (* root ratio)
                                                                           :amp (wrap-at i amps)} :ratio ratio}})))
                     (apply merge))]
     (timbre/info "Drone Box ratios:" (sort (map :ratio (vals config))))
     (legato/multi :drone-box config))))

(comment
  (require '[erv.utils.ratios :refer [ratios->scale]])
  (def scale (ratios->scale [1 5/4 3/2 15/8]))
  (drone-box 200 scale [0 2] [0.4 0.4])
  (drone-box 200 scale []))
