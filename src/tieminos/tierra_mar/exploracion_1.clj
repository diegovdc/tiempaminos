(ns tieminos.tierra-mar.exploracion-1
  "Panning drone"
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn plug*]]
   [tieminos.seq-utils.core :refer [** choose lin rainseq ret]]
   [tieminos.synths :refer [low]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(comment
  (user/connect)

  (defn map-outs
    "Given a sequence of outs, map a signal array to each out."
    [out-offset outs-seq sig]
    (if (and (sequential? outs-seq)
             (sequential? sig))
      (map (fn [out sig]
             (o/out:ar (oc/+ out out-offset) sig))
           outs-seq
           sig)
      (do (timbre/warn "[map-outs] `outs-seq` & `sig` are not both vectors. Resorting to default output method for current synth variation.")
          (o/out outs-seq sig))))

  (defn +outs1
    [params]
    (assoc params
           :out-offset 0
           :ugen/outs (plug* [:out-offset :outs]
                             '((fn [sig] (map-outs out-offset outs sig))))
           :outs [0 1 2 3]))

  (make-synth-fn
   'panny
   (-> {:freq 200
        :amp 0.5
        :dur 2
        :orientation 0
        :width 3}
       +outs1)
   '(-> freq
        o/saw
        (o/moog-ladder (* 3/2 freq) 0.9)
        (#(o/pan-az (count outs) %
                    (o/line 0
                            (* 2 (/ (dec (count outs))
                                    (count outs)))
                            (* 0.2 dur)
                            :action o/NO-ACTION)
                    :width (o/env-gen (o/envelope [width width 1 0.5]
                                                  [(* 0.8 dur)
                                                   (* 0.1 dur)
                                                   (* 0.1 dur)])
                                      :action o/NO-ACTION)
                    :orientation orientation))
        #_(o/free-verb 0.5 2)
        (* amp
           (o/amp-comp freq)
           (o/env-gen (o/env-perc 0.5 0.5)
                      :time-scale dur
                      #_(o/envelope [0 1 1 0]
                                    [0.1 (- dur 0.2) 0.1])
                      :action o/FREE))
        :ugen/outs)
   {:reset? true})

  (panny
   {:out-offset 19
    :outs [1 1]})
  (panny
   {:out-offset 19
    :outs [1 2 8 16 22 22 22]})
  (panny
   {:out-offset 19
    :width 5
    :outs [1 3 10 17 22]})
  (panny
   {:dur 10
    :width 5
    :out-offset 19
    :outs [0 1 3 4 12 13 20 22 17 9 3 4 1 0]})
  (panny
   {:dur 5
    :out-offset 19
    :width 5
    :outs [22 16 17 4 1 15]})
  (low :out 20))

(comment
  (rain.v2/ref-rain
   :id ::1
   :tempo 90
   :ratio 1
   :durs [1]
   :on-event (rain.v2/on-event
              (when (> (rand) 1)
                #_(panny
                   {:freq (* 200 (rainseq (** [#_2/3 1 {2 4 4 1 1/4 1} 1/2 #_{3/2 5 3 2}]
                                              (lin (lin 1 3/2 {7/6 11 #_#_11/7 2})
                                                   (lin 7/4 (lin 9/8 13/11 27/16))))))
                    :amp (rainseq  (** [4 3 4] [1 0.8 1.2 0.7 1]))
                    :dur (rainseq (** {4 4 2 1} [[2 5] 1 2 {4 3 1 5} 1 2] [2 3 4 1]))
                    :out-offset 19
                    :width (rainseq {5 10
                                     1.2 1
                                     3 2})
                    :orientation (rand 3)
                    :outs (rainseq (choose
                                    (ret 22 16 17 4 1 15)
                                    (ret 0 1 3 4 12 13 20 22 17 9 3 4 1 0)
                                    (apply ret (reverse [0 1 3 4 12 13 20 22 17 9 3 4 1 0]))
                                    (ret 1 3 10 17 22)
                                    (ret 7 8 14 7 6 5)
                                    (ret 2 3 5 9 10)
                                    (ret 22 16 15 7 6 1)
                                    (apply ret (range 10 15))
                                    (apply ret (reverse (range 10 15)))))})))))


