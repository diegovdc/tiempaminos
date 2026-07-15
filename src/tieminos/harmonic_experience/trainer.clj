(ns tieminos.harmonic-experience.trainer
  (:require
   [erv.utils.conversions :as conv :refer [midi->cps]]
   [erv.utils.core :refer [interval round2]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.lattice :refer [get-lattice-atom!]]
   [tieminos.lattice.v1.lattice :refer [add-played-ratio
                                        remove-all-played-ratios
                                        remove-played-ratio]]
   [tieminos.seq-utils.core :refer [choose mseq]]
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

(comment

  (let [x (choose 1 2 3)]
    (doseq [i (range 10)]
      (println (mseq i x)))))

(defn trainer
  [{:keys [root
           scale
           degrees
           durs
           periods
           lattice?
           amp
           out
           print-info?
           on-note-play]
    :or {root (midi->cps 60)
         durs (choose 5 8 10)
         periods (choose 1/2 1 2)
         lattice? true
         print-info? true
         amp 1
         out 0
         on-note-play (fn [_data] nil)}}]

  (timbre/info "Starting trainer")
  (let [last-interval (atom '(1 1))]
    (ref-rain
     :id ::trainer
     :durs (fn [{:keys [index]}] (mseq index durs))
     :on-event
     (on-event
      (let [note (nth scale (mseq i degrees))
            _ (swap! last-interval
                     #(->> (conj % (:bounded-ratio note))
                           (take 2)))
            interval* (apply interval (sort < @last-interval))
            freq (* root (mseq index periods) (:bounded-ratio note))
            a 6
            r 6]
        (when print-info?
          (timbre/info :note
                       (:ratio note)
                       (round2 2  (conv/ratio->cents (:ratio note))))
          (timbre/info :interval
                       interval*
                       (round2 2 (conv/ratio->cents interval*))
                       "\n"))
        (timbre/debug {:freq freq :ratio (:bounded-ratio note) :root root})
        (when lattice?
          (let [lattice-atom (get-lattice-atom!)]
            (add-played-ratio lattice-atom {:ratio (:bounded-ratio note)
                                            :stroke-weight 10
                                            :color [200 0 120]})

            (ref-rain :id (random-uuid)
                      :durs [(+ a r) 1]
                      :on-event (on-event
                                 (when (= i 1)
                                   (remove-played-ratio
                                    lattice-atom
                                    {:ratio (:bounded-ratio note)}))))))
        (tuning-monitor
         :freq freq
         :a a
         :r r
         :pan (rrange -0.5 0.5)
         :amp (* amp (rrange 0.4 0.8))
         :out out)

        (on-note-play {:last-interval @last-interval
                       :note note
                       :interval interval*
                       :freq freq}))))))

(defn stop []
  (gp/stop ::trainer)
  (timbre/info "Starting trainer")
  (remove-all-played-ratios (get-lattice-atom!)))
