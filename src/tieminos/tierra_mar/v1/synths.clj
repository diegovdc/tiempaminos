(ns tieminos.tierra-mar.v1.synths
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.math.utils :refer [normalize]]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn]]
   [time-time.standard :refer [rrand]]))

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
   :pan-dur-till-last 0.8
   :width-durs [0.3 0.4 0.2 0.1]
   :ugen/panner #_'((fn [sig] (map-outs out-offset outs sig)))
   '((fn [sig]
       (when (sequential? sig)
         (timbre/warn "panaz-lin expects a mono signal, received multichannel"))
       (o/pan-az (count outs)
                 sig
                 (o/env-gen
                  (let [last-az-point (/ (dec (count outs))
                                         (count outs))]
                    (o/envelope [0 (* 0.9 last-az-point) last-az-point]
                                [(* pan-dur-till-last dur) (* (- 1 pan-dur-till-last) dur)]))
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

(defplug env
  #{:dur}
  {:env-levels [0 1 1 0]
   :env-durs [0.1 0.6 0.4]
   :ugen/env '(o/env-gen (o/envelope env-levels env-durs)
                         :time-scale dur
                         :action o/FREE)})

(make-synth-fn
 'rama
 (-> {:in 0
      :amp 0.5
      :dur 2
      :asr [0.1 0.6 0.3]
      :curve 0}
     panaz-line
     +outs1)
 '(-> (o/sound-in in)
      #_(o/moog-ladder (* 3/2 freq) 0.9)
      #_(o/free-verb 0.5 2)

      :ugen/panner
      (* amp
         (o/env-gen #_(o/env-perc 0.5 0.5)
          (o/envelope [0 1 1 0] asr curve)
                    :time-scale dur
                    :action o/FREE))
      :ugen/outs
      #_(#(o/out 0 %)))
 {:reset? true})

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
   {:out-offset 0 #_(bh/bus 68)
    :dur 10
    :min-width 4
    :max-width 10
    :asr (normalize [0.1 0.02 3])
    :curve [-1 4]
     ;; :width-durs [0.05 0.15 0.1 0.7]
    :outs (range 4)}))

(make-synth-fn
 'cristal-liquidizado-2
 (-> {:buf 0
      :buf-pos 0
      :rate 1
      :amp 0.5
      :pan 0
      :dur 1
      :rev-mix 0.5
      :rev-room 1
      :delay-time 0.3
      :delay-dcy 0.7
      :delay-amp 1}
     env
     panaz-line
     +outs1)
 '(-> (o/play-buf 1 buf rate :start-pos buf-pos)
      :ugen/filter
      :ugen/panner
      (#(+ % (-> %
                 (o/comb-l delay-time delay-time delay-dcy)
                 (o/moog-ladder 1000 0.7)
                 (* delay-amp))))
      #_(* (o/env-gen (o/envelope [0 1 1 0] [0 0.5 0.5])
                      :time-scale (* 2/3 dur)))
      (o/free-verb rev-mix rev-room)
      (* amp :ugen/env)
      :ugen/outs)
 {:reset? true})


