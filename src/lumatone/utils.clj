(ns lumatone.utils)

(defn multichan-mapper
  "Replicates the functionality of Pianoteq and Surge XT for mapping multichannel MIDI values into a specific scale octave and degree.
  In Pianoteq and Surge the next chan means and period up and viceversa."
  [{:keys [period/size offset] :or {offset 0}}
   {:keys [channel note]}]
  (+ note offset (* (dec channel) size)))
