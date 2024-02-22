(ns tieminos.impros.jan042024
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [erv.scale.core :as scale]
   [erv.utils.core :refer [round2]]
   [overtone.core :as o]
   [tieminos.midi.core :refer [add-synth remove-synth synths]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.synths :refer [low short-plate2]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  add-synth
  remove-synth
  synths)

(defn add-synth* [degree synth]
  (add-synth {:note degree} synth))

(defn remove-synth* [degree]
  (remove-synth o/ctl {:note degree}))

(def root 16)

(oe/defsynth organ
  [freq 130
   amp 1
   a 2
   s 1
   r 2
   gate 1]
  (o/out 0
         (-> (* 0.6 (o/sin-osc [freq
                                (* 2 freq)
                                (* 3 freq)
                                (* 4 freq)
                                (* 5 freq)
                                (* 6 freq)
                                (* 7 freq)]))
             (o/pan2 (lfo 0.2 -1 1))
             #_(o/hpf 700)
             #_(#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (o/mix)
             (* amp (o/amp-comp (* 3 freq))
                (o/env-gen (o/env-asr a s r)
                           :gate gate
                           :action o/FREE)))))

(defn- ratios->scale [ratios]
  (map (fn [r] {:bounded-ratio r
                :bounding-period 2})
       ratios))

(def meta-slendro
  (ratios->scale [1 65/64 265/256 1081/1024 9/8 37/32 151/128 77/64 21/16 43/32
                  351/256 3/2 49/32 25/16 51/32 7/4 57/32 465/256]))

(def wilson-metameantone-19
  (ratios->scale [67/64 279/256 9/8 75/64 39/32 5/4 167/128 87/64 45/32
                  187/128 3/2 25/16 417/256 27/16 7/4 233/128 15/8 125/64 1]))

(defonce now-playing (atom {}))
(comment
  (reset! now-playing {})
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
                              (mapcat (fn [x] (map #(* x %) (range 1 9))))
                              (#(combo/combinations % 2))
                              (map (fn [[a b]] (round2 3 (abs (float (- a b))))))
                              (filter #(<= % 20))
                              frequencies
                              (sort-by (juxt second first))
                              reverse)]
                 (println d)))))
(comment
  (mod 39 19)
  (let [scale meta-slendro
        degrees (concat #_(map #(+ -4 %) #{50 60 82 83})
                 #_(map #(+ 10 (* 2 (count scale)) %) #{0 11})
                 (map #(+ 16 (* 3 (count scale)) %) #{0 11}))
        synths-degs (keys @synths)
        degs-to-remove (set/difference (set synths-degs) (set degrees))]
    (doseq [d degrees]
      (when-not (get @synths d)
        (let [freq (scale/deg->freq scale root d)
              ratio (-> scale (nth (mod d (count scale))) :bounded-ratio)]
          (println "Adding" (double freq))
          (swap! now-playing assoc d {:ratio ratio :freq freq})
          (add-synth* d
                      (organ {:freq freq
                              :amp (rrange 0.1 0.2)
                              :a 20
                              :r 5})))))

    (when (seq degs-to-remove)
      (apply swap! now-playing dissoc degs-to-remove))
    (doseq [d degs-to-remove]
      (println "Removing" degs-to-remove)
      (remove-synth* d))))

(gp/stop)
(comment
  (let [scale meta-slendro]
    (ref-rain :id :perc
              :durs [3 1 3 1 3 2]
              :ratio 1/16
              :on-event (on-event
                         (let [degrees [(at-i [0 5 -18]) 0 11 (at-i [0 5 0]) 0 14 (at-i [0 22 30])]
                               freq (scale/deg->freq scale root
                                                     (at-i degrees)
                                                     :period 4)
                               s (rand-nth [low short-plate2])
                               oct (at-i [2 1 1 1 2])]
                           (s :freq (* oct freq)
                              :amp (* 0.5 (at-i [0.5]) (/ 1 oct))
                              :atk (at-i [0.01 0.12])
                              :dcy (* oct (at-i [1 1 1 1 0.2 2]))))))
    (ref-rain :id :perc2
              :ref :perc
              :durs [1]
              :ratio 2
              :on-event (on-event
                         (let [degrees (into (repeat 15 nil) (range 20 38 4))
                               deg (at-i degrees)
                               freq (when deg (scale/deg->freq scale root
                                                               deg
                                                               :period 4))
                               s (rand-nth [low short-plate2])
                               oct (at-i [2])]
                           (when deg
                             (print "p")
                             (s :freq (* oct freq)
                                :amp (*  0.7 (at-i [0.5 0.8 0.5]) (/ 1 oct))
                                :atk (at-i [0.2])
                                :dcy (* oct (at-i [1 1 1 1 0.2 2])))
                             (when (> (rand) 0.5)
                               (s :freq (* oct (scale/deg->freq scale root
                                                                (+ 1 deg)
                                                                :period 4))
                                  :amp (*  0.7 (at-i [0.5 0.8 0.5]) (/ 1 oct))
                                  :atk (at-i [0.2])
                                  :dcy (* oct (at-i [1 1 1 1 0.2 2]))))))))))
