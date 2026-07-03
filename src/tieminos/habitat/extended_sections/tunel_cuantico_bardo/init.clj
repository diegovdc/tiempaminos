(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.init
  (:require
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.main-4ch :as hunu.4ch]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls :as bardo.live-ctl]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :as bardo.rec]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management :as bardo.synth-management]
   [tieminos.habitat.init :as habitat]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :as habitat.route]))

(defn habitat! []
  (when @habitat/habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {}))

  (habitat/init! {:volume-db -18}))

(defn inputs-4ch-gtr&mics-1&2!
  []
  (hunu.4ch/open-inputs-with-rand-pan*
   {:inputs habitat.route/inputs
    :preouts habitat.route/preouts}
   {:mic-1 {:width 3 :amp 1}
    :mic-2 {:width 3 :amp 1}
    :guitar {:width 3 :amp 1}}))

(defn all!
  []
  (habitat!)
  (inputs-4ch-gtr&mics-1&2!)
  (bardo.synth-management/periodically-clear-currently-playing-synths! (* 60 1000))
  (bardo.comms/init-async-coms! #'bardo.live-ctl/event-handler)
  (bardo.rec/init-bufs-watch!))
