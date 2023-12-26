(ns tieminos.lumatone.improvs.metameantone-organ
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as convo]
   [erv.utils.core :refer [round2]]
   [overtone.core :as o]
   [tieminos.lumatone.utils :refer [multichan-mapper]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-lumatone! midi-in-event]]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(def wilson-metameantone-19
  (->> [67/64
        279/256
        9/8
        75/64
        39/32
        5/4
        167/128
        87/64
        45/32
        187/128
        3/2
        25/16
        417/256
        27/16
        7/4
        233/128
        15/8
        125/64
        1]
       sort
       (map (fn [r] {:bounded-ratio r :bounding-period 2}))))

(o/defsynth organ
  [freq 130
   amp 1
   a 2
   s 1
   r 2
   gate 1]
  (o/out 0
         (-> (* 0.6 (o/mix (o/sin-osc [freq
                                       (* 2 freq)
                                       (* 3 freq)
                                       (* 4 freq)
                                       (* 5 freq)])))
             (o/pan2 (lfo 0.2 -0.5 0.5))
             #_(o/hpf 700)
             #_(#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (* amp (o/amp-comp (* 3 freq)) (o/env-gen (o/env-asr a s r) :gate gate :action o/FREE)))))

(defonce now-playing (atom {}))

(comment

  (add-watch now-playing ::played-ratios
             (fn [_ _ _ data]
               (println "New Chord:")
               (println (->> data
                             vals
                             (map :ratio)
                             set
                             sort))
               (doseq [d (->> data
                              vals
                              (map :freq)
                              (mapcat (fn [x] (map #(* x %) (range 1 6))))
                              (#(combo/combinations % 2))
                              (map (fn [[a b]] (round2 3 (abs (float (- a b))))))
                              (filter #(<= % 40))
                              frequencies
                              (sort-by (juxt second first))
                              reverse)]
                 (println d))))
  (def lumatone (get-lumatone!))
  (o/stop)
  (when lumatone
    (midi-in-event
     :midi-input lumatone
     :note-on (fn ([ev]
                   (let [degree (multichan-mapper {:period/size 19}
                                                  ev)
                         ratio (-> wilson-metameantone-19 (nth (mod degree 19)) :bounded-ratio)
                         freq (scale/deg->freq wilson-metameantone-19
                                               (convo/midi->cps 0)
                                               degree
                                               {:period 3})]
                     (swap! now-playing assoc (:note ev) {:ratio ratio :freq freq})
                     (organ :freq freq
                            :amp (linexp* 0 127 0.5 2 (:velocity ev))
                            :a 8
                            :r 5))))

     :note-off (fn ([ev] (swap! now-playing dissoc (:note ev)))))))
