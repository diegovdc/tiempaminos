(ns tieminos.impros.sept262023
  (:require
   [overtone.music.pitch :refer [note]]
   [piratidal.core :refer [speed]]
   [piratidal.experiments.rhythm-generator.v1 :refer [dur tidal]]
   [tieminos.synths :as synths]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(comment
  (def gen (tidal :x (dur "1 2 3 4")))
  (doseq [_ (range 10)]
    (println (gen {})))
  (gp/stop)

  (let [notes (tidal :xn (dur "<200*3 <300 400> 500>"))]
    (ref-rain
     :id :prueba-pirarhythms
     :durs (tidal :x (-> (dur "1 <1 2> <2 1> 1")
                         (speed "<1 <2 1 [3 1]> 1 1>")))
     :tempo 120
     :ratio 1/2
     :on-event (on-event
                (synths/low (notes {:index index :ratio (at-index [1 2 3])}))))))
