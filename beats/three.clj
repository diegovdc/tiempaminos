(ns beats.three
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [erv-fib-synth.compositions.garden-earth.misterioso :refer [misterioso]]
            [erv-fib-synth.midi.core :refer [all-notes-off midi-in-event]]
            [erv-fib-synth.midi.mpe :as mpe :refer [algo-note get-cps-pitch-class]]
            [erv.cps.core :as cps]
            [erv.utils.core :as u]
            [overtone.midi :as midi]
            [taoensso.timbre :as timbre]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(def outy (midi/midi-out "VirMIDI"))
(def hex (cps/make 2 [1 3 5 7]))
(def eik (->> [1 3 5 7 9 11]
              (cps/make 3)
              cps/+all-subcps))

(comment
  )

#_(on-event (println dur))
(def seq-1
  "Misterioso -- on full eikosany
  (#{1 3 11} #{7 1 11} #{3 11 9} #{7 1 9} #{7 1 5} #{7 11 9} #{7 11 5})"
  [0 4 3 -1 2 8 11])

(def seq-2
  "Darker -- on full eikosany
  (#{1 11 9} #{1 3 9} #{1 11 5} #{3 11 9} #{7 9 5})"
  [-8 -6 -5 3 25])

(defn has-set? [set* degree]
  (= set* (set/intersection (:set degree) set*)))

(def chord-1 (->> eik :scale (filter (partial has-set? #{11 7}))))

(comment
  (->> seq-1
       (map #(u/wrap-at % (:scale eik)))
       (map :set))
  (->> eik :scale
       (filter (partial has-set? #{11 7}))
       ))



(comment
;;;  test
  (algo-note :sink outy
             :dur 5000 ;; milliseconds
             :scale (:scale hex)
             :base-freq 440
             :get-pitch-class get-cps-pitch-class
             :deg-offset -5
             :midi-note 4
             :vel 20)

  (gp/stop)
  (mpe/all-notes-off outy)

  (misterioso)
  (gp/stop :claro)
  (gp/stop)
  (gp/reset)

  (claro :moment :claro :vel-amp 0.7 :offset 3
         :remove-set #{9} :id :c-2)
  (claro :moment :oscuro :vel-amp 0.6 :set* #{11 9} :offset -5
         :remove-set #{:n})
  (claro :moment :oscuro :vel-amp 0.6 :set* #{11 9} :offset -4
         :remove-set #{:n})
  (claro :moment  :oscuro :vel-amp 1.5 :set* #{7 9} :offset -4
         :remove-set #{:n})
  (gp/stop :sets-3-11-and-3-5)
  (gp/stop :claro)
  (gp/stop)
  (gp/reset)
  (all-notes-off outy)
  (->> eik :scale (filter (partial has-set? #{3 11})))

)


(comment
  "Melody exploration"
  (->> eik cps/+all-subcps :subcps keys sort (filter #(str/includes? % "2)5")))
  (defn sub-cps-scale [cps-name] (-> eik cps/+all-subcps :subcps (get cps-name) :scale))


  (midi-in-event
   :note-on (fn [msg]
              (print "note on")
              (mpe/mpe-note-on :sink outy
                               :scale eik-1
                               :base-freq 30
                               :get-pitch-class get-cps-pitch-class
                               :deg-offset -20
                               :midi-note (msg :note)
                               :vel (msg :velocity)))
   :note-off #(mpe/mpe-note-off outy (% :note)))
  (gp/stop)
  (def dek-1 "2)5 of 3)6 1-3.5.7.9.11")
  (def dek-3 "2)5 of 3)6 3-1.5.7.9.11")
  (def dek-5 "2)5 of 3)6 5-1.3.7.9.11")
  (def dek-7 "2)5 of 3)6 7-1.3.5.9.11")
  (def dek-9 "2)5 of 3)6 9-1.3.5.7.11")
  (def dek-11 "2)5 of 3)6 11-1.3.5.7.9")
  (def deks (map (fn [cps-name] [cps-name
                                (sub-cps-scale cps-name)])
                 [dek-1 dek-3 dek-5 dek-7 dek-9 dek-11]))
  (gp/stop)
  (-> deks)
  (ref-rain
   :id :auto-dekanies
   :tempo 900
   :durs [130 130 210]
   :on-event (on-event
              (let [[cps-name scale]
                    (u/wrap-at index deks)]
                (timbre/info "SETTING new CPS" cps-name "\n\n\n\n\n\n")
                (midi-in-event
                 :note-on (fn [msg]
                            (mpe/mpe-note-on :sink outy
                                             :scale scale
                                             :base-freq 30
                                             :get-pitch-class get-cps-pitch-class
                                             :deg-offset -20
                                             :midi-note (msg :note)
                                             :vel (msg :velocity)))
                 :note-off #(mpe/mpe-note-off outy (% :note)))))))
