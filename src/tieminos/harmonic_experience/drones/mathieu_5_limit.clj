(ns tieminos.harmonic-experience.drones.mathieu-5-limit
  "Full tuning for the tuning of lattice of The Harmonic Experience"
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.core :refer [pow]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone harmonic]]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :refer [trainer]]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
   [tieminos.midi.core]
   [time-time.dynacan.players.gen-poly :as gp]))

#_(def ref-note 52)
(def ref-note 48)
(def root (midi->cps ref-note))

(def harmonic-experience-12-tone
  (ratios->scale
   [1     ;; sa
    16/15 ;; komal re
    9/8   ;; re
    6/5   ;; komal ga
    5/4   ;; ga
    4/3   ;; ma
    45/32 ;; ma
    3/2   ;; pa
    8/5   ;; komal dha
    5/3   ;; dha
    9/5   ;; komal nise
    15/8  ;; ni
    ]))

(def harmonic-experience-22-tone
  "Example 16.20"
  (ratios->scale
   (set (concat
         (map #(* 5 5 (pow 3 %)) (range -1 1))
         (map #(* 5 (pow 3 %)) (range -2 4))
         (map #(pow 3 %) (range -3 6))
         (map #(* 1/5 (pow 3 %)) (range -1 3))
         [1/25]))))

(def configs
  {:oxygen {:ref-note 48
            :root root
            :scale harmonic-experience-12-tone}

   :lumatone {:ref-note 16
              :root 1
              :scale harmonic-experience-22-tone}})

(comment
  (o/stop)
  (gp/stop)
  (def kb-config :oxygen)
  (when-let [config (configs kb-config)]
    (hexp.lattice/setup-kb
     (assoc config
            :midi-kb (if (= kb-config :oxygen)
                       (tieminos.midi.core/get-oxygen!)
                       (tieminos.midi.core/get-lumatone!)))))

  (trainer (assoc (configs :oxygen) :root (midi->cps 48) :degrees [0 2 3 5 7 8 10]))
  (hexp.trainer/stop)
  (def sa (drone root))
  (o/ctl sa :amp 0.5)
  (o/ctl sa :gate 0)
  (def sa2 (drone (* 2 root)))
  (o/ctl sa2 :gate 0)
  (def pa (drone (* 3/2 root) :amp 1))
  (o/ctl pa :gate 0)
  (o/ctl pa :amp 0.6)
  (def ma (drone (* 8/3 root) :amp 0.6))
  (o/ctl ma :gate 0)

  (def h (harmonic (* root 9/8)))
  (o/ctl h :gate 0))
