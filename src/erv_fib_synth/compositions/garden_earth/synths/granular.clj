(ns erv-fib-synth.compositions.garden-earth.synths.granular
  (:require [overtone.core :as o]))

;;;;;;;;;;
;;; Synths
;;;;;;;;;

(o/defsynth grain
  ;; NOTE  OPTIMAL duration according to Eli Fieldsteel {:trigger 40 :dur 1/20}
  [buf 0
   dur 1
   trig-rate 40
   grain-dur 1/20
   rate 1
   amp 1
   pan 0
   start 0
   end 1
   out 0]

  (o/out out
         (-> (o/grain-buf
              :num-channels 2
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos (o/line start end dur o/FREE)
              :pan pan)
             (* amp))))
(comment
  (def testsound (o/load-sample "/home/diego/Desktop/happy-song-mono.wav"))
  (grain testsound
         (:duration testsound)
         40
         1/20
         :amp 3))

;;;;;;;;;;;;;;
;;; Recording
;;;;;;;;;;;;;;

(defonce bufs (atom {}))

(defn alloc [key seconds]
  (let [buf (o/buffer (* seconds (o/server-sample-rate)))]
    (swap! bufs assoc key buf)
    buf))


(o/defsynth writer [buf 0 seconds 5]
;; https://depts.washington.edu/dxscdoc/Help/Classes/RecordBuf.html
  (let [in (o/sound-in 0)
        env (o/env-gen (o/envelope [0 1 1 0]
                                   [0.01 (- seconds 0.02) 0.01]))]
    (o/record-buf:ar (* env in) buf :action o/FREE :loop 0)))

(defn rec-buf [buf-key seconds]
  (let [buf (alloc buf-key seconds)]
    (writer buf)
    buf))

(defn free-buffers []
  (doseq [b (vals @bufs)]
    (o/buffer-free b)))

(comment
  (rec-buf :hola 10)
  (o/demo 5 (o/play-buf 1 (@bufs :hola))))
