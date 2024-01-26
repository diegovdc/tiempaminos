(ns tieminos.piraran.kbm
  (:require
   [clojure.java.io :as io]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]))

(-> polydori-v2)

(def hexany-template
  "! Template for a keyboard mapping
!
! Size of map. The pattern repeats every so many keys:
6
! First MIDI note number to retune:
0
! Last MIDI note number to retune:
127
! Middle note where the first entry of the mapping is mapped to:
%s
! Reference note for which frequency is given:
%s
! Frequency to tune the above note to
%s
! Scale degree to consider as formal octave (determines difference in pitch
! between adjacent mapping patterns):
29
! Mapping.
! The numbers represent scale degrees mapped to keys. The first entry is for
! the given middle note, the next for subsequent higher keys.
! For an unmapped key, put in an \"x\". At the end, unmapped keys may be left out.
%s
%s
%s
%s
%s
%s
")

(defn get-tuning-freq
  [root-freq scale first-degree]
  (double (* root-freq (:bounded-ratio (nth scale first-degree)))))

(comment
  (let [root-freq 200
        middle-note 60]
    (->> dorian-hexanies-in-polydori
         (map (comp #(apply
                      format
                      hexany-template
                      middle-note
                      middle-note
                      (get-tuning-freq root-freq (:scale polydori-v2) (first %))
                      %) sort :degrees)) (partition 3)
         (map-indexed
          (fn [i group]
            (map-indexed (fn [j kbm]
                           [(format "/Users/diego/Music/tunings/7D-perc-ensemble/diat%sv%s-cps-4_7-1_3_7_9_15_19_21_p2@root-%shz.kbm"
                                    i
                                    (inc j)
                                    root-freq)
                            kbm]) group)))
         flatten
         (partition 2)
         (#(doseq [[file content] %]
             (io/make-parents file)
             (spit file content))))))
