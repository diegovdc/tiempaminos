(ns tieminos.compositions.garden-earth.trainer
  (:require
   [erv.scale.core :refer [+names]]
   [tieminos.compositions.garden-earth.base
    :refer
    [base-freq
     on-event
     pitch-class->pr-fingering
     ref-rain
     refrains
     subcps]]))

(do
  (def *1oo4
    ["1)4 of 3)6 1.11-3.5.7.9"
     "1)4 of 3)6 3.11-1.5.7.9"
     "1)4 of 3)6 5.11-1.3.7.9"
     "1)4 of 3)6 7.11-1.3.5.9"
     "1)4 of 3)6 9.11-1.3.5.7"
     "1)4 of 3)6 1.9-3.5.7.11"        ; difcil
     "1)4 of 3)6 3.9-1.5.7.11"
     "1)4 of 3)6 5.9-1.3.7.11"
     "1)4 of 3)6 7.9-1.3.5.11"
     "1)4 of 3)6 1.7-3.5.9.11"
     "1)4 of 3)6 3.7-1.5.9.11"
     "1)4 of 3)6 5.7-1.3.9.11"
     "1)4 of 3)6 1.5-3.7.9.11"
     "1)4 of 3)6 3.5-1.7.9.11"
     "1)4 of 3)6 1.3-5.7.9.11"])

  (def *3oo4
    ["3)4 of 3)6 1.3.5.11"
     "3)4 of 3)6 1.3.5.7"
     "3)4 of 3)6 1.3.5.9"
     "3)4 of 3)6 1.3.7.11"
     "3)4 of 3)6 1.3.7.9"
     "3)4 of 3)6 1.3.9.11"
     "3)4 of 3)6 1.5.7.11"
     "3)4 of 3)6 1.5.7.9"
     "3)4 of 3)6 1.5.9.11"
     "3)4 of 3)6 1.7.9.11"
     "3)4 of 3)6 3.5.7.11"
     "3)4 of 3)6 3.5.7.9"
     "3)4 of 3)6 3.5.9.11"
     "3)4 of 3)6 3.7.9.11"
     "3)4 of 3)6 5.7.9.11"])
  (def scale (+names base-freq (subcps (*1oo4 7))))
  (def scale (subcps "3)5 of 3)6 1.5.7.9.11"))
  (count (set (mapcat subcps *3oo4)))
  (-> scale))

(comment
  (ref-rain
   :id ::trainer
   :durs [1]
   :on-event
   (on-event
    (let [note (or #_(nth scale 1  (rand-nth [2 1 0]))
                   (rand-nth scale))]
      (tuning-monitor                   ; synth
       (* (rand-nth [ 220 880 440]) (:bounded-ratio note))
       5 5 0.25)
      (println (pitch-class->pr-fingering
                (-> note :pitch :class))
               "\n\n")
      (swap! (@refrains ::trainer) assoc :durs [(+ 0.5 (rand 3))]))))
  (swap! (@refrains ::trainer) assoc :index 0))
