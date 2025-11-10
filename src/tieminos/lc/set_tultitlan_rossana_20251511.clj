(ns set-tultitlan-rossana-20251511
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.seq-utils.core :refer [** lin rainseq]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2 :refer [on-event ref-rain]]))

(def ^:private samples-path "/Users/diego/Music/samples/field-recordings/")

(defn- load-sample
  [sample]
  (o/load-sample (str samples-path sample)))

(oe/defsynth splay
  [buf 0
   rate 1
   start-pos 0
   pan 0
   hpf-freq 40
   bpf-freq 1000
   bpf-rq 1
   rmix 0
   rroom 1
   amp 1
   a 0.1
   s 2
   r 2
   out 0]
  (let [sig (o/play-buf 2 buf
                        :rate rate
                        :start-pos start-pos
                        :loop true)]
    (o/out out (-> (rand-nth sig)
                   (o/hpf hpf-freq)
                   (o/bpf bpf-freq bpf-rq)
                   (o/free-verb rmix rroom)
                   (o/pan2 pan)
                   (* amp (o/env-gen (o/envelope
                                      [0 1 1 0]
                                      [a s r])
                                     :action o/FREE))))))

(declare jilguero-et-al)

(defn xicalco-loop
  [id]
  (ref-rain
   :id id
   :durs [1]
   :on-event (on-event
              (splay {:buf jilguero-et-al
                      :rate (rainseq [1 2 3 4 5])
                      :pan (rrange -1 1)
                      :amp (o/db->amp 24)}))))

(defn descripcion
  "Aves en bosque. Funciona mejor en las `octave`s 1/4-1/16, sobre todo 1/8 y 1/16"
  [{:keys [id octave]}]
  (let [octave (or octave 1/8)]
    (ref-rain
     :id id
     :durs [3]
     :on-event (on-event
                (splay {:buf jilguero-et-al
                        :rate (* octave (rainseq [1 2 3 4 5 [7 7/4]]))
                        :s (rainseq {4 8
                                     7 3})
                        :a (rainseq {0.4 5 2 1})
                        :rmix (rrange 0 1)
                        :bpf-freq (rainseq {1000 10
                                            (rrange 800 13000) 5})
                        :rroom (rrange 0 (rainseq {2 1
                                                   1 2
                                                   0.8 2
                                                   0.5 1}))
                        :start-pos (+ 1960410 ;; posición de aves
                                      (rainseq (** {-1 1, 1 1}
                                                   {0 10, (rrange 0 48000) 3})))
                        :pan (* 0.8 (rrange -1 1))
                        :amp (o/db->amp 36 #_(rrange 24 36))})))))

(comment ;; init
  (user/connect)
  (def jilguero-et-al (load-sample "rossana-lara_xicalco-mañana-jilguero-y-otras-aves.wav"))
  (def agua-orilla (load-sample "rossana-lara_agua-orilla-sutil.wav")))

(comment ;; exploration controls
  (rain.v2/stop)
  (o/stop)
  (user/rec "agua-demo")
  (user/rec-stop) (user/rec-stop))

(comment ;; exploration: descripción
  (def next-pos (-> jilguero-et-al :n-samples rand-int))
  (-> next-pos)
;; => 1960410 - aves
  (ref-rain
   :id ::test
   :durs [3]
   :on-event (on-event
              (splay {:buf jilguero-et-al
                      :rate (rainseq (** 1/4 [1 2 3 4 5 [7 7/4]]))
                      :s (rainseq {4 8
                                   7 3})
                      :a (rainseq {0.4 5 2 1})
                      :rmix (rrange 0 1)
                      :bpf-freq (rainseq {1000 10
                                          (rrange 800 13000) 5})
                      :rroom (rrange 0 (rainseq {2 1
                                                 1 2
                                                 0.8 2
                                                 0.5 1}))
                      :start-pos (+ next-pos
                                    (rainseq (** {-1 1, 1 1}
                                                 {0 10, (rrange 0 48000) 3})))
                      :pan (* 0.8 (rrange -1 1))
                      :amp (o/db->amp 36 #_(rrange 24 36))}))))

(comment ;; exploration
  (def next-pos (-> agua-orilla :n-samples rand-int))
  (-> next-pos)
  ;; => 4744222
  (ref-rain
   :id ::test-agua
   :durs [1]
   :ratio 1/8
   :on-event (on-event
              (splay {:buf agua-orilla
                      :rate (rainseq (** (repcat [16 -1]
                                                 #_[16 -1]
                                                 #_[12 1]
                                                 #_[8 -1]
                                                 #_[8 1]
                                                 #_[8 -1])
                                         [1 2 (lin 3 1 3) 2]))
                      :s 0.1
                      :a 0.01
                      :r (*  0.1)
                      :hpf-freq (rainseq [800 900 1200 1600])
                      :rmix (rrange 0.1 0.6)
                      :bpf-freq (rainseq (** [[1 5] 1 2 (lin 1 4) 3] [(lin 1000 1000 2000) 500]))
                      :bpf-rq 1
                      :rroom (rrange 0 (rainseq {10 1
                                                 2 1
                                                 1 2
                                                 0.8 2
                                                 0.5 1}))
                      :start-pos (+ 4744222 100)
                      :pan (+  (* 0.3 (rrange -1 1))
                               #_(rainseq [-1 1 -1 1 -1]))
                      :amp (* 3
                              (rainseq [0.9 (lin 1 2) 1.1 1.7])
                              (o/db->amp #_36 (rrange 24 36)))})))
  (def next-pos2 (-> agua-orilla :n-samples rand-int))
  (-> next-pos2)
;; => 5643180
;; => 2307721
;; => 3944756
  (rain.v2/stop ::test-agua-2)
  (ref-rain
   :id ::test-agua-2
   :ref ::test-agua
   :durs #_[4 2 2] [2 2 2 1 2 2 2 1 2]
   :ratio 1/8
   :on-event (on-event
              (when (#{0 1 2 3 4 5 6 7 8} (mod i 9))
                (splay {:buf agua-orilla
                        ;; :out (+ 19 3)
                        :rate (rainseq (** (repcat [16 -1]
                                                   #_[16 -1]
                                                   #_[8 1]
                                                   #_[8 -1]
                                                   #_[8 1]
                                                   #_[8 -1])
                                           [1 2 (lin 3 1 3 4) 2]))
                        :s (*  1/2 (rainseq [(lin 0.1 0.1 0.1 0.2) 0.01]))
                        :a 0.0
                        :r (* 1 0.01)
                        ;; :hpf-freq (rainseq [800 900 1200 1600])
                        :rmix (rrange 0.1 0.3)
                        :bpf-freq (max 20 (+ -0 (rainseq [80 100 [120 140]])))
                        :bpf-rq 0.1
                        :rroom (rrange 0 (rainseq {10 1
                                                   2 1
                                                   1 2
                                                   0.8 2
                                                   0.5 1}))
                        :start-pos (+ next-pos2 -10)
                        :pan 0
                        :amp (*
                              (rainseq [0.9 (lin 1 1.5) 1.1 1.4])
                              (o/db->amp 54 #_(rrange 24 36)))}))))
  ;; => 2928766
  (ref-rain
   :id ::test-agua-nice
   :durs [3]
   :on-event (on-event
              (splay {:buf agua-orilla
                      :rate (rainseq (** 1/4 [1 2 3 4 5 [7 7/4]]))
                      :s (rainseq {4 8
                                   7 3})
                      :a (rainseq {0.4 5 2 1})
                      :rmix (rrange 0 1)
                      :bpf-freq (rainseq {1000 10
                                          (rrange 800 13000) 5})
                      :rroom (rrange 0 (rainseq {2 1
                                                 1 2
                                                 0.8 2
                                                 0.5 1}))
                      :start-pos (+ 2928766
                                    (rainseq (** {-1 1, 1 1}
                                                 {0 10, (rrange 0 48000) 3})))
                      :pan (* 0.8 (rrange -1 1))
                      :amp (* 0.8 (o/db->amp 48 #_(rrange 24 36)))}))))
