(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scsyndefs.core
  (:require
   [tieminos.sc-utils.synths.scsyndef :refer [make-synth]]))

(def amp-regulator-replier
  (make-synth
   {:synthdef-path "/Users/diego/Music/code/tieminos/src/tieminos/habitat/extended_sections/hacia_un_nuevo_universo/scsyndefs/amp-regulator-replier.scsyndef"
    :name "amp-regulator-replier"
    :params {:in 0
             :replyRate 10
             :peakLag 3}}))
