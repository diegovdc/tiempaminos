(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-1
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [vel deg->freq diat->polydori-degree init! mempan my-malgo out
            pbell root ssuave surge-suave synths]]
   [tieminos.midi.core :refer [all-notes-off]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))
