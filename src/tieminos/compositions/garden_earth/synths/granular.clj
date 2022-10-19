(ns tieminos.compositions.garden-earth.synths.granular
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [tieminos.compositions.garden-earth.base :refer [dur->env]]
   [tieminos.compositions.garden-earth.synths.live-signal :refer [lfo]]
   [tieminos.overtone-extensions :as oe]))

(comment
  ;; load buffers for testing
  (require '[tieminos.compositions.garden-earth.synths.recording :as rec])
  (def test-samples (rec/load-own-samples!)))

;;;;;;;;;;
;;; Synths
;;;;;;;;;;

(comment
  (o/defsynth x [a 100] (o/out 0 (o/sin-osc a)))
  (x {:a 200})
  (o/stop))

(defmacro granulator-pos [buf speed start end pos-noise-freq pos-noise-amp]
  `(oc/+
    (o/lin-lin (o/lf-noise1 ~pos-noise-freq)
               -1 1
               (oc/* -1 ~pos-noise-amp)
               ~pos-noise-amp)
    (oc// (o/phasor:ar 0
                       (oc/* ~speed (o/buf-rate-scale:kr ~buf))
                       (oc/* ~start (oc/- (o/buf-samples:kr ~buf) 1))
                       (oc/* ~end (oc/- (o/buf-samples:kr ~buf) 1)))
          (o/buf-samples:kr ~buf))))
(o/defsynth grain
  ;; NOTE  OPTIMAL duration according to Eli Fieldsteel {:trigger 40 :dur 1/20}
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur [grain-dur]
              :sndbuf buf
              :rate rate
              :pos (granulator-pos buf speed start end
                                   pos-noise-freq pos-noise-amp)
              :pan (lfo 0.1 -1 1))
             (* amp
                (o/env-gen (o/envelope [0 1 1 0] [a (- dur a r) r])
                           :action o/FREE)))))
(o/defsynth lines
  ;; NOTE  OPTIMAL duration according to Eli Fieldsteel {:trigger 40 :dur 1/20}
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (let [sig (o/grain-buf
             :num-channels 2
             :trigger (o/impulse trig-rate)
             :dur [grain-dur 1/10]
             :sndbuf buf
             :rate (+ rate #_(lfo 0.1 -0.1 0.1))
             :pos (o/line start end dur)
             #_(granulator-pos buf speed start end
                               pos-noise-freq pos-noise-amp)
             :pan (lfo 0.1 -1 1))]
    (o/out out
           (-> sig
               (o/hpf (lfo 0.5 2000 5000))
               (o/rlpf (lfo 0.15 50 10000) 0.2)
               (+ (* sig (o/sin-osc (lfo 2 16 200)
                                    :mul (lfo 2.5 0.1 0.3))))
               (* amp
                  (o/sin-osc)
                  (lfo 0.1 0.4 1)
                  (o/env-gen (o/envelope [0 1 1 0] [a (- dur a r) r])
                             :action o/FREE))))))

(o/defsynth dot
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur [grain-dur]
              :sndbuf buf
              :rate rate
              :pos (granulator-pos buf speed start end
                                   pos-noise-freq pos-noise-amp)
              :pan (lfo 0.1 -1 1))
             (* amp
                (o/env-gen (o/perc dur r)
                           :action o/FREE)))))

(comment
  (def testsound (o/load-sample "/home/diego/Desktop/happy-song-mono.wav"))
  (-> testsound)
  (o/stop)
  (grain {:buf testsound
          :dur (:duration testsound)
          :trig-rate 100
          :grain-dur 1/80
          :speed 6000
          :amp 3
          :mix 1})
  (let [b (@bufs ["D#+75" :a])]
    (grain-lfo b
               (:duration b)
               40
               1/20
               #_#_#_#_:start 0.3
                   :end 0.31
               :amp 3
               :mix 1))
  (-> @bufs keys)
  (o/demo 5 (o/play-buf 1 (@bufs ["D#+75" :a]))))

(o/defsynth grain-lfo
  ;; NOTE  OPTIMAL duration according to Eli Fieldsteel {:trigger 40 :dur 1/20}
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos (granulator-pos buf speed start end
                                   pos-noise-freq pos-noise-amp)
              :pan (lfo 0.1 -1 1))
             (o/free-verb mix room damp)
             (o/lpf (lfo 0.5 20 8000))
             (* (lfo amp-lfo min-amp amp)
                (o/env-gen (o/envelope [0 1 1 0] [a (- dur a r) r])
                           :action o/FREE)))))
(o/defsynth grain-perc
  ;; NOTE  OPTIMAL duration according to Eli Fieldsteel {:trigger 40 :dur 1/20}
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur [grain-dur]
              :sndbuf buf
              :rate rate
              :pos (granulator-pos buf speed start end
                                   pos-noise-freq pos-noise-amp)
              :pan (lfo 0.1 -1 1))
             (o/free-verb mix room damp)
             (o/lpf (lfo 0.5 20 8000))
             (* (lfo amp-lfo min-amp amp)
                (o/env-gen (o/env-perc a r)
                           :action o/FREE)))))

(comment
  (def testsound (o/load-sample "/home/diego/Desktop/happy-song-mono.wav"))
  (o/stop)
  (grain testsound
         10
         40
         1/20
         #_#_#_#_:start 0.3
             :end 0.31
         :amp 3
         :mix 1)
  (let [b (@bufs ["D#+75" :a])]
    (grain-lfo b
               (:duration b)
               40
               1/20
               #_#_#_#_:start 0.3
                   :end 0.31
               :amp 3
               :mix 1))
  (-> @bufs keys)
  (o/demo 5 (o/play-buf 1 (@bufs ["D#+75" :a]))))

(oe/defsynth ocean
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   panl -1
   panr 1
   pan-lfo 0.1
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos (granulator-pos buf speed start end
                                   pos-noise-freq pos-noise-amp)
              :pan (lfo pan-lfo panl panr))
             (o/lpf 3000)
             (o/free-verb mix room damp)
             (* (lfo amp-lfo 0.1 amp)
                (o/env-gen (o/envelope [0 1 1 0] [a (- dur a r) r])
                           :action o/FREE)))))

(oe/defsynth ocean-2
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   speed 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0
   end 1
   a 0.1
   r 0.1
   mix 0.5
   room 0.5
   damp 0.3
   panl -1
   panr 1
   pan-lfo 0.1
   pos-noise-freq 100
   pos-noise-amp 0.05
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos (granulator-pos buf speed start end
                                   pos-noise-freq pos-noise-amp)
              :pan (lfo pan-lfo panl panr))
             (o/pluck :decaytime 2)
             #_(o/distortion2 3) ;; broken for some reason, but distortion2 works by itself (on further inspection seems like input needs to be mono)
             (o/distort)
             (o/free-verb mix room damp)
             (* (lfo amp-lfo 0.1 amp)
                (o/env-gen (o/envelope [0 1 1 0] [a (- dur a r) r])
                           :action o/FREE)))))

(oe/defsynth sample&hold
  [buf 0
   dur 1
   trig-rate 100
   grain-dur 1/10
   rate 1
   amp 1
   min-amp 0.5
   amp-lfo 0.1
   start 0.1
   end 0.3
   a 0.1
   d 1
   d-level 0.3
   r 3
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur [grain-dur]
              :sndbuf buf
              :rate rate
              :pos (o/line start end (+ a d r))
              :pan (lfo 0.1 -1 1))
             (o/rlpf (lfo 0.1 300 5000) 0.3)
             (* amp
                (o/env-gen (o/envelope [0 1 d-level 0] [a d r]
                                       [-1 -5])
                           :action o/FREE)))))

(comment
  (let [[_ b] ((vec @rec/bufs) 8)]
    (apply sample&hold
           :buf b :trig-rate 100 :grain-dur 1/10
           :start 0.5 :end 0.1 :d-level 0.3
           (merge {:trig-rate 100 :grain-dur 1/10}
                  (dur->env {:a 1 :d 2 :r 10} 10)))))
