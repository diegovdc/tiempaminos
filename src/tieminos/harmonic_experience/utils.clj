(ns tieminos.harmonic-experience.utils
  (:require
   [erv.scale.core :as scale :refer [interval->ratio]]
   [erv.utils.core :refer [interval]]
   [tieminos.blackhole :as bh]))

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

(defonce
  ^{:doc "Should be nil or :reaper"}
  output-mode (atom nil))

(defn set-output-mode!
  "Should be nil or :reaper"
  [mode]
  (reset! output-mode (if (= mode :reaper) :reaper nil)))

(defn out
  [bh-out]
  (if (= :reaper @output-mode)
    (bh/bus bh-out)
    0))

(comment
  (require '[erv.utils.ratios :refer [ratios->scale]])
  (def scale (ratios->scale [1 5/4 3/2 15/8]))
  (drone-box {:root 200 :scale scale :degrees [0 2] :amps [0.4 0.4]})
  (stop-drone-box))
