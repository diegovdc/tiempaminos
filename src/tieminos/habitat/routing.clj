(ns tieminos.habitat.routing
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.overtone-extensions :refer [defsynth]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [ctl-synth ctl-synth2]]))

(comment
  ;;  USAGE
  (defsynth input
    [in 0 out 0]
    (o/out out (o/sound-in in)))
  (def guitar (input (:guitar ins) 0))
  (o/ctl guitar :out (reaper-returns 2))
  (o/stop))

(def reaper-returns
  "Returns map, numbered after the returns in reaper i.e. 1 based numbers"
  (let [starting-chan 29
        n-chans 4
        total-returns 4
        return-outs (range starting-chan
                           (+ starting-chan
                              (* n-chans total-returns))
                           n-chans)]
    (into {} (map-indexed (fn [i out] [(inc i) out]) return-outs))))

(def processes-return-1
  "Return for buffers and sounds processed by SC"
  (reaper-returns 3))
;;;;;;;;;;;
;; Buses ;;
;;;;;;;;;;;


(defonce guitar-bus (o/audio-bus 1 "guitar-bus"))
(defonce mic-1-bus (o/audio-bus 1 "mic-1-bus"))
(defonce mic-2-bus (o/audio-bus 1 "mic-2-bus"))
(defonce mic-3-bus (o/audio-bus 1 "mic-3-bus"))
(defonce mic-4-bus (o/audio-bus 1 "mic-4-bus"))
(defonce mic-5-bus (o/audio-bus 1 "mic-5-bus"))

;;;;;;;;;;;;
;; inputs ;;
;;;;;;;;;;;;


(defsynth input
  [in 0 out 0]
  (o/out out (o/sound-in in)))

(def ins
  "Input name to blackhole channels map"
  {:guitar 20
   :mic-1  21
   :mic-2  22
   :mic-3  23
   :mic-4  24
   :mic-5  25})

(def inputs
  {:guitar {:in (:guitar ins)
            :bus guitar-bus}
   :mic-1 {:in (:mic-1 ins)
           :bus mic-1-bus}
   :mic-2 {:in (:mic-2 ins)
           :bus mic-2-bus}
   :mic-3 {:in (:mic-3 ins)
           :bus mic-3-bus}
   :mic-4 {:in (:mic-4 ins)
           :bus mic-4-bus}
   :mic-5 {:in (:mic-5 ins)
           :bus mic-5-bus}})

(def input-number->bus*
  {0 guitar-bus
   1 mic-1-bus
   2 mic-2-bus
   3 mic-3-bus
   4 mic-4-bus
   5 mic-5-bus})

(defn input-number->bus [n]
  (if-not (number? n)
    n
    (get input-number->bus* n n)))

(defn bus->bus-name [bus]
  (-> bus :name (str/replace #"-bus" "")))

;;;;;;;;;;;;;;
;; preouts ;;;
;;;;;;;;;;;;;;


(defsynth preout
  [in 0
   ;; return 1
   clean-return-amp 1.5
   ;; return 2
   rev-return-amp 1.5
   gate 1
   release 5]
  (let [env (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                       gate
                       :action o/FREE)]
    (o/out (reaper-returns 1)
           (* env clean-return-amp (o/in in 4)))
    (o/out (reaper-returns 2)
           (* env clean-return-amp (o/in in 4)))))

(defonce preouts (atom {}))

(defn set-preout-in!
  [input-name in-bus]
  (swap! preouts #(-> %
                      (assoc-in [input-name :bus] in-bus)
                      (update-in [input-name :synth] ctl-synth :in in-bus))))

(defn init-preouts!
  [inputs]
  (doseq [[_ {:keys [bus synth]}] @preouts]
    (o/free-bus bus)
    (ctl-synth2 synth :gate 0))
  (reset! preouts
          (->> inputs
               (map (fn [[input-name _]]
                      (let [preout-bus (o/audio-bus 4 (str "preout-" (name input-name)))]
                        [input-name {:bus preout-bus
                                     :synth (preout {:group (groups/late)
                                                     :in preout-bus})}])))
               (into {}))))

(comment
  (o/stop)
  (init-preouts! inputs)
  (-> @preouts :guitar :bus)
  (defsynth sini [out 0]
    (o/out out (* 0.2 (o/sin-osc [200 210 220 230]))))
  (def busy (o/audio-bus 4 "busy"))
  (sini {:group (groups/early) :out busy})
  (preout {:group (groups/late)
           :in busy})
  (outy {:group (groups/late)
         :in busy})

  (o/stop)

  (defsynth outy [in 0]
    (let [in* (o/in in 4)]
      (o/out (reaper-returns 1) in*)
      (o/out (reaper-returns 2) in*))))
