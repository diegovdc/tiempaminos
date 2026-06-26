(ns tieminos.harmonic-experience.drone-box
  (:require
   [erv.scale.core :as scale :refer [deg->freq]]
   [erv.utils.conversions :refer [midi->cps]]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.drones.sounds :refer [drone]]
   [tieminos.harmonic-experience.sustainer :as legato]
   [tieminos.utils :refer [wrap-at]]))

(defn start
  ([{:keys [root scale degrees amps out]
     :or {root (midi->cps 60)
          amps [0.7]
          out 0}}]
   (let [config (->> degrees
                     (map-indexed
                      (fn [i deg]
                        (let [ratio (:bounded-ratio (wrap-at deg scale))
                              freq (deg->freq scale root deg)]
                          {deg {:inst drone
                                ::ratio ratio
                                :params {:freq freq
                                         :amp (wrap-at i amps)
                                         :out out}}})))
                     (apply merge))]
     (timbre/info "Drone Box ratios:" (sort (map ::ratio (vals config))))
     (legato/multi :drone-box config))))

(defn stop
  []
  (timbre/info "Stopping Drone Box")
  (legato/multi-stop :drone-box))
