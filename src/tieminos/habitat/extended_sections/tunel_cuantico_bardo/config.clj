(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.config)

(def reaper-tracks*
  ;; TODO: `:guitar` and `:mic-1` and `:mic-2` keys are used for other stuff, so can't namespace them right now as with the above. Ideally they could all be namespaced.
  {:guitar 3 ;; line-in
   :guitar/mic 4
   :mic-1 6
   :mic-2 7
   :guitar-input-track 3
   :guitar-clean-track 16
   :guitar-processes-track 17
   :percussion-processes-track 20
   :mixes-processes-2-track 22
   :eq-track 26})
