(ns tieminos.harmonies.experiments.2026-10-01

  (:require [erv.cps.core :as cps]
            [tieminos.harmonies.cps-patterns :refer [cps-seq readable]]
            [tieminos.seq-utils.core :refer [** lin rainseq]]
            [tieminos.synths :as s]
            [tieminos.utils :refer [wrap-at]]
            [time-time.dynacan.players.refrain.v2 :as rain.v2]
            [time-time.standard :refer [rrand]]))

(comment
  (require '[clojure.string :as str])
  ;; cycles from https://archive.org/details/combination-product-sets-cycles
  (def eik-triad-cycles
    ["abc bcf bcd bdf cdf def cef cde ace bce abc"
     "abc abd acd ade cde def cdf bdf bcf abf abc"
     "abc bcf abf acf adf acd ade ace abe bce abc"
     "abc bcf bce bef abe bde ade abd acd bcd abc"
     "abc abd acd ade cde def cdf bdf bcf bcd abc"
     "abc acf abf aef bef def bde ade abd acd abc"])
  (->
   "A.B.C - B.C.F - B.C.D - B.D.F - C.D.F - D.E.F - C.E.F - C.D.E - A.C.E - B.C.E - A.B.C
A.B.C - A.B.D - A.C.D - A.D.E - C.D.E - D.E.F - C.D.F - B.D.F - B.C.F - A.B.F - A.B.C
A.B.C - B.C.F - A.B.F - A.C.F - A.D.F - A.C.D - A.D.E - A.C.E - A.B.E - B.C.E - A.B.C
A.B.C - B.C.F - B.C.E - B.E.F - A.B.E - B.D.E - A.D.E - A.B.D - A.C.D - B.C.D - A.B.C
A.B.C - A.B.D - A.C.D - A.D.E - C.D.E - D.E.F - C.D.F - B.D.F - B.C.F - B.C.D - A.B.C
A.B.C - A.C.F - A.B.F - A.E.F - B.E.F - D.E.F - B.D.E - A.D.E - A.B.D - A.C.D - A.B.C"
   (str/split #"\n")
   (->> (map #(-> % (str/replace "." "")
                  (str/replace "- " "")
                  (str/lower-case))))))
(comment

  (rain.v2/stop)
  (let [pat (->> (cps-seq
                  (cps/make 3 [1 31 7 9 13 23])
                  (rotate 0 "abc bcf-1 bcd bdf-1 cdf def cef cde ace bce")))
        freqs (->freqs 200 pat)]

    (println (readable pat) #_(->freqs 1 pat))
    (rain.v2/ref-rain :id ::test, :ratio 1/3, :durs [2 3 3],
                      :on-event (rain.v2/on-event
                                 (when-not (#{0} (mod i 5))
                                   (s/noise-tone (*  (at-i freqs)
                                                     2
                                                     (rainseq [1 [1 2] [1/2 [1 1/2]] 1]))
                                                  ;; :atk (rrand 0.5 2)
                                                 :dcy (rrand  2 4)
                                                 :amp (* 0.5 (at-i [1.5 1 1 1])))
                                   (when (#{1 3} (mod i 5))
                                     (s/short-plate (*  (at-i freqs)
                                                        4
                                                        (rainseq [1 [1 2] [1/2 [1 1/2]] 1]))
                                                    :atk (rrand 0.1 2)
                                                    :mod-freq (* 4 (at-i freqs))
                                                    :dcy (rrand  2 4)
                                                    :amp (* 0.5 (at-i [1.5 1 1 1])))))
                                 (if (#{0 6 8} (mod i 9))
                                   (let [p (rainseq [4 4 1/2 4 2])
                                         dist (rainseq [-1 -2 -3 -4])]
                                     (rain.v2/ref-rain
                                      :id (random-uuid)
                                      :ratio (rainseq (lin  1/6 1/3 1/6 2/3 1/6))
                                      :loop? false
                                      :durs (map-indexed #(* %2 #_(Math/pow 1.2 %1)) [1 1 1 1 1 1 1])
                                      :on-event (rain.v2/on-event
                                                 ((at-i [s/low s/sharp-plate]) (*  (wrap-at (+ dist 2 i) freqs)
                                                                                   p
                                                                                   (rainseq (** [1 [1 2] [1/2 [1 1/2]] 1]
                                                                                                {1 8
                                                                                                 2 1/2
                                                                                                 1/2 1/2})))
                                                     ;; :atk (rrand 0.5 2)
                                                                               :dcy (rrand 0.3 1)
                                                                               :amp (* 0.5 (at-i [1.5 1 1 1]))))))
                                   (s/short-plate (*  (wrap-at (+ 2 i) freqs)
                                                      (rainseq [2 4 2])
                                                      (rainseq [1 [1 2] [1/2 [1 1/2]] 1]))
                                                   ;; :atk (rrand 0.5 2)
                                                  :dcy (rrand  2 4)
                                                  :amp (* 0.5 (at-i [1.5 1 1 1]))))
                                 (when-not (#{0} (mod i 7))
                                   (s/low (*  (wrap-at (+ 1 i) freqs)
                                              (rainseq [1 [1 2] [1/2 [1 1/2]] 1]))
                                          :atk (rrand 0.5 2)
                                          :dcy (rrand 0.5 2)
                                          :amp (* 0.5 (at-i [1.5 1 1 1]))))))))
