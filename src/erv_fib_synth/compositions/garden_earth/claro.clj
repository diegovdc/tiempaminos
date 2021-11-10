(ns erv-fib-synth.compositions.garden-earth.claro
  (:require
   [erv-fib-synth.compositions.garden-earth.base
    :refer [eik midi-out-1 get-cps-pitch-class algo-note wrap-at
            on-event ref-rain stop has-set?]]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [taoensso.timbre :as timbre]))

(defn claro
  "o 2 3 | t 2 3
       t 2 3 | 1 t 3
       G y F
       1 2 3m| 1 t t
       1 2 3 | 1+>2>+>3 8va F-> Fd

  Ideally the sets should be the mayor chord ones #{11 7} #{9 7} #{11 9}
  "
  [& {:keys [moments vel-amp id offset set* remove-set tempo-ratio
             durs]
      :or {id :claro
           offset -6
           set* #{11 7}
           remove-set #{:nothing}
           tempo-ratio 1
           durs [30000 20000]}}]
  (let [chord (->> eik :scale
                   (filter (partial has-set? set*))
                   (remove (partial has-set? remove-set)))]
    #_(timbre/info "Claro" moment (mapv :set chord))
    (timbre/info "Claro" moments set* offset)
    (ref-rain
     :id id
     :tempo 900
     :durs [14 17 14 17 17]
     :ratio tempo-ratio
     :on-event (on-event
                (when-let [{:keys [scale note vel]}
                           (case (wrap-at index moments)
                             :claro {:scale chord
                                     :note (wrap-at index [3 4 5 6 0 1 2])
                                     :vel (->> [7 10 7 18 3]
                                               (wrap-at index)
                                               (* vel-amp)
                                               int)}
                             ;; la oscuridad de la claridad
                             :oscuro {:scale chord
                                      :note (wrap-at index [-2 13 12 -7 10 -3 11])
                                      :vel (->> [7 10 7 18 3]
                                                (wrap-at index)
                                                (* vel-amp)
                                                int)})]
                  #_(timbre/info
                     id set*
                     "pitch"
                     (conv/cps->name* (scale/deg->freq
                                       chord
                                       440
                                       (+ note offset)))
                     "vel" vel)

                  (algo-note :sink midi-out-1
                             :dur (wrap-at index durs) ;; millisecons
                             :scale scale
                             :get-pitch-class get-cps-pitch-class
                             :deg-offset offset
                             :midi-note note
                             :vel vel))))))

(comment
  (stop)
  (claro :moment :claro :vel-amp 0.7 :offset 3
         :remove-set #{9} :id :c-2 :tempo-ratio 9/7)
  (claro :moment :oscuro :vel-amp 0.6 :set* #{11 9} :offset -5
         :remove-set #{:n})
  (claro :moment :oscuro :vel-amp 0.6 :set* #{11 9} :offset -4
         :remove-set #{:n})
  (claro :moment  :oscuro :vel-amp 1.5 :set* #{7 9} :offset -4
         :remove-set #{:n}))
