(ns tieminos.habitat.scratch.test-sample-rec
  (:require
   [clojure.string :as str]
   [tieminos.habitat.recording :as rec]
   [tieminos.sc-utils.recording.v1 :as recutils]
   [tieminos.habitat.routing :as r]
   [time-time.standard :refer [rrand]]))

(comment
  (defn input-name [input-bus]
    (-> input-bus :name (str/replace #"-bus" "")))

  (do
    (def section "amanecer")
    (def subsection "pt7")
    (defn input-config [input-bus]
      {:section section
       :subsection subsection
       :input-name (input-name input-bus)
       :input-bus input-bus
       :dur-s (rrand 8.0 15)}))

  (rec/rec-input (input-config r/guitar-bus))
  (rec/rec-input (input-config r/mic-1-bus))
  (rec/rec-input (input-config r/mic-2-bus))
  (rec/rec-input (input-config r/mic-3-bus))
  (rec/rec-input (input-config r/mic-4-bus))
  ;; remove last sample of pt1 gtr mic1
  (-> @rec/bufs)
  (rec/save-samples :full-keyword :habitat/test-samples-v1)

  (def test-samples
    (recutils/load-own-samples!
     :buffers-atom (atom {})
     :prefixes-set #{:habitat/test-samples-v1}
     :samples-path rec/habitat-samples-path))
  (-> @test-samples))
