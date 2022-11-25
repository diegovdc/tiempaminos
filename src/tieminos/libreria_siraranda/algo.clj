(ns tieminos.libreria-siraranda.algo
  (:require
   [overtone.core :as o]
   [overtone.midi :as midi]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn- midi-algo-note-fn
  [sink dur note vel & {:keys [offset]
                        :or {offset 0}}]
  (let [note* (+ offset note)]
    (ref-rain
     :id (random-uuid)
     :durs [dur 0.1]
     :loop? false
     :on-event (on-event
                (if (zero? index)
                  (midi/midi-note-on sink note* vel)
                  (midi/midi-note-off sink note*))))))

(defn midi-algo-note
  [sink dur note vel & {:keys [offset]
                        :or {offset 0}}]
  (if (sequential? note)
    (doseq [n note]
      (midi-algo-note-fn
       sink dur n vel :offset offset))
    (midi-algo-note-fn
     sink dur note vel :offset offset)))

(comment
  (def outy (midi/midi-out "VirMIDI"))
  (o/stop)
  (gp/stop)

  (ref-rain
   :id :siranda
   :ratio 3
   :durs [3 2 2]
   :on-event (on-event
              (midi-algo-note
               outy
               (+ 10 dur)
               (at-index [79
                          (- 79 18)

                          (- 79 36)])
               (at-index [10])
               :offset 0)))
  (ref-rain
   :id :siranda2
   :ratio 1
   :durs [3 1/3 1/3]
   :on-event (on-event
              (midi-algo-note
               outy
               (+  dur)
               (at-index [(- 66 36)
                          (- 66 11)])
               (at-index [30 40])
               :offset (at-index [0 5])))))

(comment
  (def outy (midi/midi-out "VirMIDI"))
  (o/stop)
  (gp/stop)

  (ref-rain
   :id :ejercicio
   :ratio 1/16
   :durs [3 2 2]
   :on-event (on-event
              (midi-algo-note
               outy
               (+ 2 dur)
               (at-index [30 60 [61 31] 67 [71 (at-index [66 73 75])]])
               (at-index [10 12
                          (at-i [0 0 5 5 7])
                          10 (at-i [12 0]) 1])
               :offset 0)))
  (ref-rain
   :id :ejercicio*
   :ref :ejercicio
   :ratio 1
   :durs [3 2 2 3 1 1]
   :on-event (on-event
              (midi-algo-note
               outy
               10
               (at-index [80 [81 70] 90 91])
               (at-index [5 80 5 5 5 5 5])))))

