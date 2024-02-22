(ns tieminos.habitat.resonance-panner
  (:require
   [clojure.math :refer [round]]
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.habitat.groups :as groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]))

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
          high 2500
          verb-in (o/free-verb in (lfo-kr 0.1 0.4 0.8) (lfo-kr 0.4 0.5 0.9) (lfo-kr 0.4 0 0.9))]
      (o/out out (->
                  (+ (* 1.2
                        (o/resonz verb-in
                                  (o/i-rand low high)
                                  (lfo-kr 0.3 0.01 0.4)))
                     (* (lfo-kr (o/i-rand 0.01 0.1) 0 1.4)
                        (o/ringz verb-in
                                 (o/i-rand low high)
                                 (lfo-kr 0.25 0.01 0.3))))
                  (o/free-verb (lfo-kr 0.1 0.4 0.6) (lfo-kr 0.4 0.5 0.7) (lfo-kr 0.43 0 0.9))
                  ;; TODO put reverb in delay?
                  (#(o/pan-az 4 % (lfo-kr 0.2 -1 1) :width 0.5 :orientation (lfo-kr 0.3 0 1)))
                  (* amp
                     (lfo-kr (o/i-rand 0.01 0.1) 0.4 1.3)
                     (o/env-gen (o/envelope [0 1 1 0] (map #(* dur %) [0.2 0.5 0.3]))
                                :action o/FREE))))))
  (comment

    (map (fn [_] (rezopan1 :group (groups/early)
                           :in 0
                           :amp 0.5
                           :dur 100
                           :out 8))
         (range 5))))

;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;

(defonce state
  (atom {}))

(defn update-state
  [k {:keys [in value] :as _args-map}]
  (swap! state assoc-in [in k] value))

(defn trigger
  [in out default-voices default-dur]
  (let [voices (get-in @state [in :voices] default-voices)
        dur (get-in @state [in :dur] default-dur)]
    (println "reso-pan" in voices dur)
    (doseq [_ (range voices)]
      (rezopan1 :group (groups/panners)
                :in in
                :amp 0.5
                :dur dur
                :out out))))
