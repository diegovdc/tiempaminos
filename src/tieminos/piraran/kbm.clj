(ns tieminos.piraran.kbm
  (:require
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
60
! Reference note for which frequency is given:
60
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

(comment
  (let [tuning-freq 261.6255653006]
    (->> dorian-hexanies-in-polydori
         (map (comp #(apply format hexany-template tuning-freq %) sort :degrees)) (partition 3)
         (map-indexed
          (fn [i group]
            (map-indexed (fn [j kbm]
                           [(format "/Users/diego/Music/tunings/piraran/diat%sv%s-cps-4_7-1_3_7_9_15_19_21_p2@%s.kbm"
                                    i
                                    (inc j)
                                    tuning-freq)
                            kbm]) group)))
         flatten
         (partition 2)
         (#(doseq [[file content] %]
             (spit file content))))))
