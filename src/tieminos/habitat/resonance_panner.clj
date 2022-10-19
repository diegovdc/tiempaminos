(ns tieminos.habitat.resonance-panner
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr lfo0-kr]]))

(comment
  (o/stop))

(do
  (oe/defsynth rezopan1
    [in 0
     out 0
     dur 5
     amp 1]
    (let [in (o/sound-in in)
          low 400
          high 2500]
      (o/out out (->
                  (+ (* 1.2
                        (o/resonz in
                                  (o/i-rand low high)
                                  (lfo-kr 0.3 0.01 0.4)))
                     (* (lfo-kr (o/i-rand 0.01 0.1) 0 1.4)
                        (o/ringz in
                                 (o/i-rand low high)
                                 (lfo-kr 0.25 0.01 0.3))))
                   ;; TODO put reverb in delay?
                  (o/free-verb (lfo-kr 0.1 0.4 0.8) (lfo-kr 0.4 0.5 0.9) (lfo-kr 0.4 0 0.9))
                  (#(o/pan-az 4 % (lfo-kr 0.1 -1 1) :width 1 :orientation (lfo-kr 0.3 0 1)))
                  (* amp
                     (lfo-kr (o/i-rand 0.01 0.1) 0.3 1.3)
                     (o/env-gen (o/envelope [0 1 1 0] (map #(* dur %) [0.2 0.5 0.3]))
                                :action o/FREE))))))
  (comment

    (map (fn [_] (rezopan1 :group (groups/early)
                           :in 0
                           :amp 0.5
                           :dur 100
                           :out 8))
         (range 5))))
