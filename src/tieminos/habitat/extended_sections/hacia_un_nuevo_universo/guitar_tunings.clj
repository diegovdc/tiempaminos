(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.guitar-tunings
  (:require
   [erv.utils.conversions :refer [cps->name*]]
   [erv.utils.core :refer [prime-factors]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.synths :refer [soft-saw2]]))

(def f 112)
(cps->name* f)
(def string->ratio {6 1
                    5 3/2
                    4 5/2
                    3 7/2
                    2 11/2
                    1 25/4             ;maybe later 25/4 or 13/2
                    })
(o/defsynth tuner
  [freq 112
   ratio 1
   amp 0.5
   atk 1
   dcy 1
   sust 1
   rel 1
   gate 1]
  (o/out 0 (* (o/pan2 (o/saw (* freq ratio)))
              amp
              (o/env-gen (o/env-adsr atk dcy sust rel)
                         :gate gate :action o/FREE))))

;;;;;;;;;;;  exploring another tuning

(comment
  (def t (tuner f (string->ratio 0)))
  (def t (tuner f 1))
  (o/ctl t :gate 0)
 (o/stop))



(cps->name* f)
(def test-scale (atom nil))
;;  v0
(let [f* 19]
  (->> (range f* (* f* 8) 4)
       #_(count)
       (map (juxt identity
                  prime-factors
                  #(-> % (* (/ f 8)) cps->name*)))
       (take 10)
       (reset! test-scale)))
;;  v1
(let [f* 19]
  (->> (range f* (* f* 8) 11)
       #_(count)
       (map (juxt identity
                  prime-factors
                  #(-> % (* (/ f 8)) cps->name*)))
       (take 10)
       (reset! test-scale)))

;; v2
(let [f* 19]
  ;; skeep note 4
  (->> (range f* (* f* 8) 12)
       #_(count)
       (map (juxt identity
                  prime-factors
                  #(-> % (* (/ f 8)) cps->name*)))
       (take 10)
       (reset! test-scale)))

(comment
  (def oxygen (get-oxygen!))
  (-> @test-scale)

  (do
    (o/ctl t :gate 0)
    (let [note 6
          note-data (nth @test-scale note)
          ratio (-> note-data  first (/ 32))]
      (println note-data)
      (def t (tuner f ratio))))
  (o/stop)

  (when oxygen
    (midi-in-event
      :midi-input oxygen
      :note-on (fn [{:keys [note]}]

                 (let [harmonic (some-> @test-scale
                                        (nth (- note 48) nil)
                                        first) ]
                   (if harmonic
                     (do (println "harmonic:" harmonic)
                         (soft-saw2 :freq (* (/ f 32) harmonic
                                             #_(rand-nth [1 2 3 4 5 6 7]))
                                    :atk 1))
                     (timbre/warn "Harmonic not found for midi note" note)))
                 )
      ))

  (o/stop)
  )

;;  v2 TODO
(let [f* 19]
  (->> (range f* (* f* 8) 11)
       #_(count)
       (map (juxt identity
                  prime-factors
                  #(-> % (* (/ f 8)) cps->name*)))
       (take 10)))
