(ns erv-fib-synth.compositions.garden-earth.fl-grain-1.parallel-lines
  (:require [erv-fib-synth.compositions.garden-earth.base
             :refer [subcps
                     ref-rain
                     on-event
                     scale->pr-fingerings
                     stop
                     interval-from-pitch-class
                     groups
                     eik]]
            [clojure.data.generators :refer [weighted]]
            [erv-fib-synth.compositions.garden-earth.synths.recording :as rec]
            [clojure.set :as set]
            [erv-fib-synth.compositions.garden-earth.synths.granular :as granular]
            [erv.utils.core :as utils]
            [taoensso.timbre :as timbre]
            [overtone.core :as o]))



(println (scale->pr-fingerings (subcps "2)4 of 3)6 1-3.5.7.11")))
(subcps "2)4 of 3)6 3-5.7.9.11")

(comment
  (stop)
  ;; DONE one part
  (let [bufs (rec/filter* (fn [[_ id]]
                            ;; (= id :oceanic) ; also works well
                            (not= id :oceanic)) @rec/bufs)
        scale (subcps "2)4 of 3)6 1-3.5.7.11")
        ba (atom (first (vec @rec/bufs)))
        line
        (concat
         [1 2 4]
         #_[5 3 5 3 2 2]
         [5 1 1 1 5 1 -5 1 2 -20 1
          1 3 5 6 7 1 1 1 1 1 1
          1 1 1 1 1 2 3 4 6 9 1
          -10 -20 8 1 6 1 5 1 7 3 1
          12 11 10 9 8 7 6 5 4 3 2 1
          15 14 13 12 11 10 9 8 7
          17 16 15 14 13]
         #_(flatten [(range -20 20)
                     (range -10 20)
                     (reverse (range 0 20))
                     (range 0 20)
                     (reverse (range -10 20))
                     (range -10 10)
                     (range 5 10)
                     (range 5 30 3)
                     (range -15 30 6)]))

        amps [0.2]
        dur-amps [1.2]]

    (ref-rain
     :id :parallel/one
     :durs [1 1 1 1 1/2 1/2 1 2]
     :tempo 20
     :ratio 1
     :on-event (on-event
                (let [[[pc t] b] (reset! ba (rand-nth (vec bufs)))
                      rate (interval-from-pitch-class
                            scale
                            pc
                            (at-index line))
                      dur* (* (at-index dur-amps) (/ dur-ms 1000))]
                  (granular/lines
                   {:out 8
                    :group [:head (@groups :early)]
                    :buf b
                    :dur dur*
                    :amp (* (at-index amps) 10)
                    :trig-rate 100
                    :grain-dur 1/10
                    :a (* dur* 0.3)
                    :rate rate
                    :start 0.1
                    :end 0.11
                    :speed 0.012}))))
    (ref-rain
     :id :parallel/three
     :ref :parallel/one
     :ratio 8/7
     :on-event (on-event
                (let [[[pc t] b] @ba
                      rate (interval-from-pitch-class
                            scale
                            pc
                            (- (at-index line) 6))
                      dur* (* (at-index dur-amps) (/ dur-ms 300))]
                  (granular/lines
                   {:out 8
                    :group [:head (@groups :early)]
                    :buf b
                    :a (* dur* 0.3)
                    :dur dur*
                    :amp (* (at-index amps) 10)
                    :trig-rate 80
                    :grain-dur 1/10
                    :rate rate
                    :start 0.1
                    :end 0.11
                    :speed 0.122}))))
    (ref-rain
     :id :parallel/two
     :ref :parallel/one
     :ratio 2/7
     :on-event (on-event
                (let [[[pc t] b] @ba
                      rate (interval-from-pitch-class
                            scale
                            pc
                            (at-index line))
                      dur* (* (at-index dur-amps) (/ dur-ms 300))]
                  (granular/lines
                   {:out 8
                    :group [:head (@groups :early)]
                    :buf b
                    :dur dur*
                    :a (* dur* 0.3)
                    :amp (* (at-index amps) 20)
                    :rate rate
                    :start 0.2
                    :end 0.21
                    :speed 0.3}))))
    #_(ref-rain
       :id :parallel/four
       :ref :parallel/one
       :ratio 3/10
       :on-event (on-event
                  (let [[[pc t] b] ((vec @rec/bufs) 3)
                        rate (interval-from-pitch-class
                              scale
                              pc
                              (+ -5 (at-index line)))]
                    (granular/lines
                     {:out 8
                      :group [:head early-g]
                      :buf b
                      :dur (* (at-index dur-amps) (/ dur-ms 300))
                      :amp (* (at-index amps) 20)
                      :rate rate
                      :start (rand 0.5)
                      :speed 0.3}))))
    #_(ref-rain
       :id :parallel/four
       :ref :parallel/one
       :ratio 1/16
       :on-event (on-event
                  (let [[[pc t] b] ((vec @rec/bufs) 3)
                        rate (interval-from-pitch-class
                              scale
                              pc
                              (+ -15 (at-index line)))]
                    (granular/lines
                     {:out 8
                      :group [:head early-g]
                      :buf b
                      :dur (* (at-index dur-amps) (/ dur-ms 300))
                      :amp (* (at-index amps) 20)
                      :rate rate
                      :start (rand 0.5)
                      :speed 0.3}))))))
