(ns tieminos.habitat.scratch.convolution
  (:require
   [overtone.core :as o]
   [tieminos.habitat.recording :as rec :refer [norm-amp]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus
                                              mic-4-bus
                                              preouts]]))

(comment
  (-> @rec/test-samples keys sort)
  (let [buf-key :amanecer-pt1-mic-2-1
        buf-key2 :amanecer-pt2-mic-3-2
        b1 (-> @rec/test-samples buf-key)
        b2 (-> @rec/test-samples buf-key2)]
    (o/demo
     5
     (o/pan2 (* 0.5 (o/convolution (* (norm-amp b2) (o/play-buf 1 b2))
                                   (* (norm-amp b1) (o/play-buf 1 b1))
                                   4096)))))
  (let [buf-key :amanecer-pt1-mic-2-1
        buf-key2 :amanecer-pt1-mic-3-2
        b1 (-> @rec/test-samples buf-key)
        b2 (-> @rec/test-samples buf-key2)]
    (o/demo
     5
     (o/pan2 (* (norm-amp b2) (o/play-buf 1 b2)))))
  (do
    (try (o/kill c) (catch Exception _ nil))
    (oe/defsynth convolver
      [buf-1 0
       buf-1-amp 0.5
       buf-2 0
       buf-2-amp 0.5]
      (o/out 0 (-> (+ (o/convolution (* buf-1-amp (lfo 0.2 0.1 0.4)
                                        (o/play-buf 1 buf-1
                                                    :rate 1
                                                    :loop true
                                                    ;; :start-pos (lfo 0.4 1000 50000)
                                                    ;; :trigger (o/dust 0.11)
                                                    ))
                                     (* buf-2-amp (lfo 0.2 0.1 0.3)
                                        (o/play-buf 1 buf-2
                                                    :start-pos (lfo 0.4 1000 50000)
                                                    :trigger (o/dust 0.1)
                                                    :rate 1
                                                    :loop true))
                                     4096))
                   #_(o/rhpf 500 0.5)
                   (o/free-verb 1 0.2)
                   (#(+ %
                        (let [bpf-sig (o/bpf % (lfo 1 100 3400) (lfo 0.3 0.03 0.5))]
                          (+ (* 0.8 bpf-sig)
                             (* 1.2 (o/free-verb bpf-sig 0.8 1))))))
                   (* 2)
                   #_(o/lpf 5000)
                   (o/pan4 (o/lf-noise1 0.3)
                           (o/lf-noise1 0.3)))))
    (let [buf-keys (-> @rec/test-samples keys sort)
          buf-key (rand-nth buf-keys) #_:amanecer-pt1-mic-2-1
          buf-key2 (rand-nth buf-keys) #_:amanecer-pt1-mic-3-2
          _ (println buf-key buf-key2)
          buf-1       (-> @rec/test-samples buf-key)
          buf-2  (-> @rec/test-samples buf-key2)]
      (def c (convolver
              {:buf-1 buf-1
               :buf-1-amp (* 0.8 (norm-amp buf-1))
               :buf-2 buf-2
               :buf-2-amp (* 0.8 (norm-amp buf-2))}))))

  (-> @rec/test-samples keys))
