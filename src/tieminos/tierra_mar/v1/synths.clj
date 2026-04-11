(ns tieminos.tierra-mar.v1.synths
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]
   [tieminos.math.utils :refer [normalize]]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn]]))

(defn map-outs
  "Given a sequence of outs, map a signal array to each out."
  [out-offset outs-seq sig]
  (if (and (sequential? outs-seq)
           (sequential? sig))
    (map (fn [out sig]
           (o/out:ar (oc/+ out out-offset) sig))
         outs-seq
         sig)
    (do (timbre/warn "[map-outs] `outs-seq` & `sig` are not both vectors. Resorting to default output method for current synth variation.")
        (o/out outs-seq sig))))

#_(defn +outs1
    [params]
    (assoc params
           :out-offset 0
           :ugen/outs (plug* [:out-offset :outs]
                             '((fn [sig] (map-outs out-offset outs sig))))
           :outs [0 1 2 3]))

(defplug +outs1
  {:out-offset 0
   :ugen/outs '((fn [sig] (map-outs out-offset outs sig)))
   :outs [0 1 2 3]})

(defplug panaz-line
  #{:outs :dur}
  {:min-width 2
   :max-width 4
   :orientation 0
   :width-durs [0.3 0.4 0.2 0.1]
   :ugen/panner #_'((fn [sig] (map-outs out-offset outs sig)))
   '((fn [sig] (o/pan-az (count outs) sig
                         (o/env-gen
                          (let [last-az-point (/ (dec (count outs))
                                                 (count outs))]
                            (o/envelope [0 (* 0.9 last-az-point) last-az-point]
                                        [(* 0.8 dur) (* 0.2 dur)]))
                          :level-scale 2
                          :action o/NO-ACTION)
                         #_(o/line 0
                                   (* 2 (/ (dec (count outs))
                                           (count outs)))
                                   (* 0.8 #_0.2 dur)
                                   :action o/NO-ACTION)
                         :width (o/env-gen (o/envelope [min-width
                                                        max-width max-width
                                                        min-width min-width]
                                                       width-durs
                                                       -2)
                                           :time-scale dur
                                           :action o/NO-ACTION)
                         :orientation orientation)))})

(comment
  ;; test
  (make-synth-fn
   'panny
   (-> {:freq 200
        :amp 0.5
        :dur 2
        :asr [0.1 0.6 0.3]
        :curve 0}
       panaz-line
       +outs1)
   '(-> freq
        o/saw
        (o/moog-ladder (* 3/2 freq) 0.9)
        #_(o/free-verb 0.5 2)
        :ugen/panner
        (* amp
           (o/amp-comp freq)
           (o/env-gen #_(o/env-perc 0.5 0.5)
            (o/envelope [0 1 1 0] asr curve)
                      :time-scale dur
                      :action o/FREE))
        :ugen/outs)
   {:reset? true})
  (o/stop)
  (panny
   {:out-offset (bh/bus 68)
    :dur 10
    :min-width 4
    :max-width 10
    :asr (normalize [0.1 0.02 3])
    :curve [-1 4]
    ;; :width-durs [0.05 0.15 0.1 0.7]
    :outs (range 40)}))
