(ns tieminos.compositions.garden-earth.synths.ps-freeze
  (:require
   [clojure.math :refer [pow]]
   [overtone.core :as o]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]))

(defn maybe-mix
  [sig]
  (if (sequential? sig)
    (o/mix sig)
    sig))

(defn maybe-wrap-in-vec
  [sig]
  (if (sequential? sig)
    sig
    [sig]))
#_(ns-unmap *ns* 'ps-freeze)
(make-synth-fn
 'ps-freeze
 (-> {:in 0
      :amp 1
      :freezed-amp 1
      :ps-amp 1
      :ps-ratios [1 3/2 5/4]
      :freeze-ratios [1 2]
      :a 1
      :r 5
      :gate 1
      :freeze-gate 0
      :freeze-a 3
      :rev-mix 0.7
      :rev-room 1
      :out 0})
 '(let [spread-ps (fn [sig ratios]
                    (->> (o/pitch-shift sig  0.5 #_(/ ratios 2) ratios)
                         maybe-wrap-in-vec
                         (map (fn [sig*] (-> sig*
                                             (* (lfo-kr (o/rand 0.3 0.7) (o/rand 0.1 0.3) (o/rand  0.5 1)))
                                             (o/pan2  (lfo-kr (o/rand 0.3 0.7) -1 1)))))
                         maybe-mix))
        sig (-> (o/sound-in in)
                (spread-ps ps-ratios)
                (o/mix))
        chain (o/fft (o/local-buf (pow 2 14))
                     (* (o/delay-l sig 0.5 0.5)
                        (o/env-gen (o/asr 0.2 1 0.2)
                                   :gate gate)))

        freezig (-> chain
                    (o/pv-mag-freeze freeze-gate)
                    (o/ifft :winsize 1))
        freezig* (spread-ps freezig freeze-ratios)
        freezig** (* (-> freezig*
                         (o/moog-ladder 2000 0.5))
                     freezed-amp
                     (lfo-kr (o/rand 0.3 0.7) 0.5 1)

                     (o/env-gen (o/asr freeze-a 1 0.1 2)
                                :gate freeze-gate))
        sig* (* sig (o/env-gen (o/asr 0.1 1 2)
                               :gate gate))]
    (o/out out (* amp
                  (-> (+ (* ps-amp  sig*)  freezig**)
                      (o/free-verb rev-mix rev-room))
                  (o/env-gen (o/asr a 1 r 2)
                             :gate gate
                             :action o/FREE)))))

(comment
  (require '[tieminos.blackhole :as bh]
           '[tieminos.sc-utils.dev :as sc.dev])
  (sc.dev/track :a (ps-freeze {:in (bh/bus 3)
                               :amp 64
                               :ps-amp 0.5
                               :ps-ratios [1 3/2 7/4]
                               :freezed-amp 16
                               :r 5
                               :out (bh/bus 24)}))
  (do (o/ctl (sc.dev/get-synth :a) :freeze-gate 1)
      (sc.dev/stop :a)))




