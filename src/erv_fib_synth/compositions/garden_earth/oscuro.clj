(ns erv-fib-synth.compositions.garden-earth.oscuro
  (:require
   [erv-fib-synth.compositions.garden-earth.base
    :refer [eik midi-out-1 get-cps-pitch-class algo-note wrap-at
            on-event ref-rain stop has-set?]]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [taoensso.timbre :as timbre]))

(defn sets-3-11-and-3-5
  "Oscuro disonante"
  [& {:keys [vel-amp id]
      :or {id :sets-3-11-and-3-5}}]
  (let [chord-1 (->> eik :scale (filter (partial has-set? #{3 11})))
        chord-2 (->> eik :scale (filter (partial has-set? #{3 5})))
        offset 0]
    (timbre/info "Oscuro")
    (ref-rain
     :id id
     :tempo 900
     :durs [3 3 5]
     :on-event (on-event
                (let [chord (if (> 0.5 (/ (mod index 20) 20))
                              chord-1 chord-2)]
                  (when-let [{:keys [scale note vel offset]
                              :or {offset offset}}
                             (case (wrap-at index [1 1 1 1
                                                   2 2 2 2])
                               1 {:scale chord
                                  :note (wrap-at index [0 2 6 -4])
                                  :vel (->> [7 10 7 18 3]
                                            (wrap-at index)
                                            (* vel-amp)
                                            int)
                                  :offset (rand-nth [0 4 -12 12])}
                               2 {:scale chord
                                  :note (wrap-at index [1 3])
                                  :vel (->> [7 10 7 18 3]
                                            (wrap-at index)
                                            (* vel-amp)
                                            int)
                                  :offset (rand-nth [0 -4 -8 8])})]
                    #_(timbre/info "pitch"
                                   (conv/cps->name* (scale/deg->freq
                                                     chord
                                                     440
                                                     (+ note offset)))
                                   "vel" vel)

                    (algo-note :sink midi-out-1
                               :dur (wrap-at index [3000 5000]) ;; millisecons
                               :scale scale
                               :get-pitch-class get-cps-pitch-class
                               :deg-offset offset
                               :midi-note note
                               :vel vel)))))))

(comment
  (gp/stop :claro)
  (sets-3-11-and-3-5 :moment :claro :vel-amp 1))
