(ns tieminos.compositions.7D-percusion-ensamble.exploration2
  "Based on excerpts from https://www.youtube.com/watch?v=_0p0Cqw-fHY"
  (:require
   [overtone.midi :as midi :refer [midi-control]]
   [tieminos.midi.core :refer [get-iac2! midi-in-event]]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.synths :refer [short-plate2]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand wrap-at]]))

(def sink (midi/midi-out "VirMIDI"))

(def iac2 (get-iac2!))

(defn bh "Blackhole outs" [out] (+ 20 out))

(defn my-malgo
  [config]
  (malgo-note (merge {:sink sink
                      :scale-size 6
                      :base-midi-deg 60}
                     config)))

;; tempo 540, bar 7/16
(comment
  (gp/stop)

  (condp contains? 1
    #{0 3 5} (short-plate2 :out (bh 0))
    nil)
  (short-plate2 :out (bh 0))

  ;; NOTE this is nice, use as a basis for a climaxy thing
  (let [m [5/4 3/2 9/8 4/3 1]
        m2 [15/8 2 10/3 1]
        m3 [3/2 6/2 3/2 9/2]
        m-i (atom 0)
        m-i2 (atom 0)
        next (fn [m m-i]
               (wrap-at (swap! m-i inc) m ))]
    (ref-rain
      :id ::1
      :durs [1]
      :tempo 540
      :on-event (on-event
                  (condp contains? (mod index 7)
                    #{0 5} (short-plate2 :freq (* 200 (next m m-i)) :out (bh 0))
                    #{3} (short-plate2 :freq (* 400 (next m m-i)) :atk 0.1 :out (bh 0))
                    nil)
                  (condp contains? (mod index 5)
                    #{ 0 2 4} (short-plate2 :freq (* 200 (next m2 m-i2))  :out (bh 0))
                    nil)
                  (condp contains? (mod index 11)
                    #{0 7 10} (short-plate2 :freq (* (at-i [200 400]) (next (at-i [m2 m3 m3]) m-i2))  :out (bh 0))
                    nil)
                  (condp contains? (mod index 13)
                    #{0 8 12} (short-plate2 :freq (* 100 (next (at-i [m2 m3]) m-i2))  :out (bh 0))
                    nil))))
  (ref-rain
    :id ::1
    :durs [3 2 1 1]
    :tempo 540
    :on-event (on-event
                (case (mod index 4)
                  0 (short-plate2 :out (bh 0))
                  1 (short-plate2 :out (bh 0))
                  2 (short-plate2 :out (bh 0))
                  3 (short-plate2 :amp 0.1 :mod-freq 200 :out (bh 0))
                  nil)))
  (ref-rain
    :id ::1
    :durs [3 2 2]
    :tempo 540
    :on-event (on-event
                (short-plate2 :out (bh 0))))
  (do
    (ref-rain
      :id ::1
      :durs [3 2 2]
      :ratio 1/9
      :on-event (on-event
                  (let [deg (at-i [0 2 1 3])]
                    (short-plate2 :out (bh 0))
                    (midi-control sink 10 (rand-int 128) 0)
                    (my-malgo {:base-midi-chan 0
                               :deg (- deg 6)
                               :dur 0.3
                               :vel 100 #_(at-i [100 80 80 60 80 127]) #_(int (* (rand) 127))}))))

    (midi-in-event
      {:midi-input iac2
       :note-on (fn [{:keys [note velocity channel]}]
                  (short-plate2 :out (bh 2)))
       :auto-ctl false}))

  (ref-rain
    :id ::1
    :durs [3 2 2]
    :ratio 1/9
    :on-event (on-event
                (let [deg (at-i [(at-i [0 1 -1 0 -6]) (at-i [2 3 8])  (at-i [1 5 1 5 1]) (at-i [9 3 4 3 3 4 3])])]
                  (my-malgo {:base-midi-chan 0
                             :deg deg
                             :dur (rrand 0.3 1.5)
                             :vel (at-i [100 105 95]) #_(at-i [100 80 80 60 80 127]) #_(int (* (rand) 127))})))))
