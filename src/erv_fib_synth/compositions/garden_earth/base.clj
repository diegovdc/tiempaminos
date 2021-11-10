(ns erv-fib-synth.compositions.garden-earth.base
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            erv-fib-synth.midi.algo-note
            erv-fib-synth.midi.core
            erv-fib-synth.midi.mpe
            [erv-fib-synth.compositions.garden-earth.synths.granular
             :as granular]
            [erv.cps.core :as cps]
            erv.utils.core
            [overtone.midi :as midi]
            [potemkin :refer [import-vars]]
            [taoensso.timbre :as timbre]
            [time-time.dynacan.players.gen-poly :as gp]))

(import-vars
 [erv-fib-synth.midi.core midi-in-event]
 [erv-fib-synth.midi.mpe get-cps-pitch-class mpe-note-on mpe-note-off]
 [erv-fib-synth.midi.algo-note algo-note]
 [erv.utils.core wrap-at]
 [time-time.dynacan.players.gen-poly on-event ref-rain])

(deref gp/refrains)

(defmacro log-with-msg [msg code]
  (when msg (timbre/info msg))
  (pprint code)
  (println "\n")
  code)

(defmacro log [code]
  (pprint code)
  (println "\n")
  code)


(defn stop
  ([refrain-key]
   (gp/stop refrain-key))

  ([]
   (gp/stop)
   (granular/free-buffers)))


(def midi-out-1 (midi/midi-out "VirMIDI"))

(def eik (->> [1 3 5 7 9 11]
              (cps/make 3)
              cps/+all-subcps))

(defn has-set? [set* degree]
  (= set* (set/intersection (:set degree) set*)))

;;;;;;;;;;;;;;;;;;;
;;;; auto-scales
;;;;;;;;;;;;;;;;;;

(defonce auto-scales (atom {}))

(defn sub-cps-scale
  [cps-name]
  (-> eik :subcps (get cps-name) :scale))

(defn const-log-fn [tabs]
  (fn [_] (str/join "\n" tabs)))


(defn run-auto-scale
  "To make a basic `log-fn`, that ignores `cps-name`, `const-log-fn` can be used"
  [sub-cps-scales key*
   & {:keys [id durs log-fn]
      :or {id :auto-scale
           durs [130 130 210]
           log-fn (fn [cps-name] "")}}]
  (let [scales (map (fn [cps-name] [cps-name (sub-cps-scale cps-name)])
                    sub-cps-scales)]
    (ref-rain
     :id id
     :tempo 900
     :durs durs
     :on-event (on-event
                (let [[cps-name scale] (wrap-at index scales)]
                  (timbre/info "CPS" index cps-name "\n"
                               (str "\n" (log-fn cps-name))
                               "\n\n\n\n")
                  (swap! auto-scales assoc key* scale))))))
