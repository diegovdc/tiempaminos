(ns tieminos.harmonic-experience.trainer
  (:require
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [interval round2]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.lattice :refer [get-lattice-atom!]]
   [tieminos.lattice.v1.lattice :refer [add-played-ratio
                                        remove-all-played-ratios
                                        remove-played-ratio]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(o/defsynth tuning-monitor
  [freq 440
   a 1.5
   r 3.5
   amp 0.05
   pan 0
   lpf-freq 2500
   out 0]
  (o/out out
         (-> (o/sin-osc [freq
                         (* 2 freq)
                         (* 3 freq)
                         (* 4 freq)
                         (* 5 freq)])
             (o/lpf lpf-freq)
             (o/pan2 pan)
             o/mix
             (* amp (o/env-gen (o/env-perc a r :curve 0.5)
                               :action o/FREE)))))

(defn trainer
  [{:keys [root
           scale
           degrees
           lattice?]
    :or {lattice? true}}]
  (let [last-interval (atom '(1 1))]
    (ref-rain
     :id ::trainer
     :durs (fn [_] (rand-nth [5 8 10]))
     :on-event
     (on-event
      (let [note (nth scale (rand-nth degrees))
            _ (swap! last-interval
                     #(->> (conj % (:bounded-ratio note))
                           (take 2)))
            interval* (apply interval (sort < @last-interval))
            freq (* root (rand-nth [1/2 1 2]) (:bounded-ratio note))
            a 6
            r 6]
        (timbre/info :note
                     (:ratio note)
                     (round2 2  (conv/ratio->cents (:ratio note))))
        (timbre/info :interval
                     interval*
                     (round2 2 (conv/ratio->cents interval*))
                     "\n")
        (when lattice?
          (let [lattice-atom (get-lattice-atom!)]
            (add-played-ratio lattice-atom {:ratio (:ratio note)
                                            :stroke-weight 10
                                            :color [200 0 120]})
            (ref-rain :id (random-uuid)
                      :durs [(+ a r) 1]
                      :on-event (on-event
                                 (when (= i 1)
                                   (remove-played-ratio
                                    lattice-atom
                                    {:ratio (:ratio note)}))))))
        (tuning-monitor
         :freq freq
         :a a
         :r r
         :pan (rrange -0.5 0.5)
         :amp (rrange 0.4 0.8)
         :out 0))))))

(defn stop []
  (gp/stop ::trainer)
  (remove-all-played-ratios (get-lattice-atom!)))
