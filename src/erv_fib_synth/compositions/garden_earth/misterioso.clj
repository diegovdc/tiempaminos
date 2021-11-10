(ns erv-fib-synth.compositions.garden-earth.misterioso
  (:require
   [erv-fib-synth.compositions.garden-earth.base
    :refer [eik midi-out-1 get-cps-pitch-class algo-note wrap-at
            on-event ref-rain stop]]
   [taoensso.timbre :as timbre]))


(defn misterioso [& {:keys [id vel-amp]
                     :or {id :misterioso
                          vel-amp 0.7}}]
  (timbre/info "Misterioso")
  (ref-rain
   :id id
   :tempo 900
   :durs [4 7 4 7 7]
   :on-event (on-event
              (when-let [{:keys [scale note vel]}
                         (case (wrap-at index [1])
                           1 {:scale (:scale eik)
                              :note (wrap-at index [0 4 3 -1 2 8 11])
                              :vel (wrap-at index [10 30 70])})]

                (algo-note :sink midi-out-1
                           :dur (wrap-at index [7000 10000]) ;; millisecons
                           :scale scale
                           :base-freq 440
                           :get-pitch-class get-cps-pitch-class
                           :deg-offset -6
                           :midi-note note
                           :vel (int (* vel-amp vel)))))))
