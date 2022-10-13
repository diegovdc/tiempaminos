(ns tieminos.afable-diablo.d1pt4.core
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [overtone.core :as o :refer :all]
   [time-time.dynacan.players.gen-poly :as gp :refer [ref-rain]]
   [time-time.standard :refer [rrand]]))

(def multitud1 (load-sample "/Users/diego/Music/code/tieminos/resources/multitudFeminista.wav"))
(def multitud2 (load-sample "/Users/diego/Music/code/tieminos/resources/multitudFeminista2.wav"))

(do
  (defsynth noise-tone
    [noise-in 0
     freq 200
     bwr 0.01
     pan 0
     out 0
     start-pos 0
     amp 1
     atk 0.1
     dcy 0.4]
    (o/out out
           (-> (in noise-in)
               (bpf freq bwr)
               #_(lpf (* 1.1 freq))
               (* 120 amp (env-gen (env-perc atk dcy) :action FREE))
               (distort)
               free-verb
               (pan2 pan))))

  (noise-tone))

(defsynth noise-out
  [buf 10
   out 0]
  (o/out out (play-buf 1 buf :loop true)))

(noise-out :buf multitud1 :out 10)
(noise-out :buf multitud2 :out 10)

(comment
  (stop)
  (tieminos.core/rec "piraran-pt4-sketch")
  (recording-stop)
  (gp/stop)
  (def hex (:scale (cps/make 3 [1 3 5 7])))
  (ref-rain
   :id :multitud
   :durs [3]
   :ratio 1/20
   :on-event (gp/on-event
              (noise-tone
               :bwr (rrand 0.01 0.2)
               :noise-in 10
               :freq (scale/deg->freq hex
                                      (rand-nth [50 100 200])
                                      (at-i (flatten [[6 5 3 -7 -7 -6 -10 -11]
                                                      (reverse [6 5 3 -7 -7 -6 -10 -11])])))
               :amp 0.5
               :atk (+ 0.01 (rand 2))
               :dcy (+ 0.1 (rand 3)))
              (let [degs [0 1 2]
                    dcy 0.7]
                (noise-tone
                 :bwr 0.01
                 :noise-in 10
                 :freq (scale/deg->freq hex
                                        (rand-nth [400 200])
                                        (at-i degs))
                 :amp 4
                 :atk 0.1
                 :dcy (* dcy (+ 0.2 (rand 2))))
                (noise-tone
                 :bwr 0.01
                 :noise-in 10
                 :freq (scale/deg->freq hex
                                        (rand-nth [800 400])
                                        (at-i degs))
                 :amp 3
                 :atk 0.1
                 :dcy (* dcy (+ 0.2 (rand 3))))))))
