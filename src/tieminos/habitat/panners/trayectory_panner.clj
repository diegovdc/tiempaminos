(ns tieminos.habitat.panners.trayectory-panner
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.routing :refer [reaper-returns]]
   [tieminos.math.random-walk :refer [rand-walk1]]
   [tieminos.math.utils :refer [normalize]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.habitat.groups :as groups]))

(defn trayectory-pan-4ch
  "`trayectories` is [{:pos 0 :dur 5 :width 1.2}]. The `:dur` is the transition duration (in seconds) between the current node and the next,
  and the one on the last map is not necessary.
  After peforming the trayectory the panner will stay in the last position until it is released."
  [{:keys [trayectory group in out amp a d s release gate]
    :or {trayectory [{:pos 0 :dur 1 :width 2}]
         in 0
         out 0
         amp 1
         release 2
         a 1
         d 1
         s 1
         group (groups/panners)}}]
  (when-not trayectory
    (timbre/warn "No `:trayectory` key present"))
  (let [pos-levels (map :pos trayectory)
        pos-durations (map :dur trayectory)
        widths (map :width trayectory)
        curves (repeat (count trayectory) 0.2)
        release-node (dec (count trayectory))]
    ((o/synth [gate 1]
              (o/out out
                     (-> (oe/circle-az :num-channels 4
                                       :in (o/in in)
                                       :pos (o/env-gen (o/envelope pos-levels pos-durations curves release-node))
                                       :width (o/env-gen (o/envelope widths pos-durations curves release-node))
                                       :orientation 0)
                         (* amp (o/env-gen (o/env-adsr a d s release :curve -0.5)
                                           gate
                                           :action o/FREE)))))
     group)))

(comment
  (o/demo
   5
   (oe/circle-az :num-channels 4
                 :in (o/lpf (o/saw 300) 800)
                 :pos (o/env-gen (o/envelope [0] [5]))
                 :width 1.2
                 :orientation 0))
  (groups/init-groups!)
  (def test-bus (o/audio-bus 1 "test-bus"))
  (def s ((o/synth (o/out test-bus
                          (* 0.2 (o/lpf (o/saw 300) 1500))))
          (groups/early)))

  (o/kill s)
  (o/stop)

  (def t (trayectory-pan-4ch {:out 0
                              :in test-bus
                              :trayectory [{:pos 2 :dur 5 :width 1.3}
                                           {:pos 1.5 :dur 10 :width 1.3}]}))
  (o/ctl t :gate 0)
  :trayectory-pan)

(defn trayectory-noise
  [{:keys [max-pos-change
           max-width-change
           max-dur-change
           trayectory]}]
  (map (fn [step]
         (-> step
             (update :pos + (rand (* (rand-nth [1 -1]) max-pos-change)))
             (update :width (fn [w] (-> w
                                        (+ (rand (* (rand-nth [1 -1]) max-width-change)))
                                        (max 1.1)
                                        (min 4))))
             (update :dur (fn [dur] (-> dur
                                        (+ (rand (* (rand-nth [1 -1]) max-dur-change)))
                                        (max 0.1))))))
       trayectory))

(defn random-trayectories
  [{:keys [num-steps total-dur max-pos-change initial-pos]
    :or {max-pos-change 1}}]
  (map (fn [pos dur width] {:pos pos
                            :dur (* dur total-dur)
                            :width width})
       (map #(+ % initial-pos) (rand-walk1 max-pos-change num-steps))
       (normalize (map (fn [_] (rand)) (range num-steps)))
       (map (fn [_] (+ 1.1 (rand 2.9))) (range num-steps))))

(oe/defsynth simple-perc-trayectory-4ch
  ;; This synth does not use `gate`, as other panners do
  [in 0
   start-pos 0
   end-pos 0
   curve 1
   dur 2
   start-width 1.5
   end-width 1.5
   a 1
   r 1
   amp 1
   out 0]
  (o/out out
         (-> (oe/circle-az :num-channels 4
                           :in (o/in in)
                           :pos (o/env-gen (o/envelope [start-pos end-pos] [dur] [curve]))
                           :width (o/env-gen (o/envelope [start-width end-width] [dur] [curve]))
                           :orientation 0)
             (* amp (o/env-gen (o/env-perc a r)
                               :time-scale (/ dur (+ a r))
                               :action o/FREE)))))

(comment
  (def test-bus (o/audio-bus 1 "test-bus"))
  (def s ((o/synth (o/out test-bus
                          (* 0.2 (o/lpf (o/saw 300) 1500))))
          (groups/early)))
  (o/kill s)
  (def st (simple-perc-trayectory-4ch
           {:in test-bus
            :out (reaper-returns 1)
            :start-pos 0
            :end-pos 0.5
            :curve 1.2
             ;; :start-width 2
            })))
