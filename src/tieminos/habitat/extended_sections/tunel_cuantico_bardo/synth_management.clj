(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management
  (:require
   [clojure.core.async :as a]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [time-time.standard :refer [rrand]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Currently Playing synths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: not all playing synths are being tracked at the moment, only those from algocloud
(def currently-playing-synths (atom ()))

(defn add-synth!
  [synth dur]
  (swap! currently-playing-synths
         conj
         (with-meta synth {:live-controls.synth/dur dur})))

(defn- stop-long-running-synths!*
  [min-synth-duration currently-playing-synths-data]
  ;; NOTE: if a synth is starting right now, it may not be stopped.
  (doseq [s currently-playing-synths-data]
    (let [dur (:live-controls.synth/dur (meta s))]
      (when (and (o/node-active? s)
                 dur
                 (>= dur min-synth-duration))
        (timbre/info "Stopping synth:" s)
        ;; using rrand to disperse fadeouts
        (o/ctl s :gate (rrand -6.0 -14))))))

(defn stop-long-running-synths!
  "Stops all long running synths if their total duration (not the remaining) exceed the `min-synth-duration`."
  [min-synth-duration]
  (timbre/info "Stopping long running synths")
  (stop-long-running-synths!* min-synth-duration @currently-playing-synths))

(defn clear-currently-playing-synths!
  "Remove synths that are no longer playing from the `currently-playing-synths` atom"
  []
  (swap! currently-playing-synths
         (fn [synths]
           (remove #(= :destroyed (o/node-status %))
                   synths))))

(def ^:private clear-loop-running? (atom false))

(defn periodically-clear-currently-playing-synths!
  "Starts a go-loop that clears synth references periodically if they have already been destroyed.
  The purpose is to prevent memory leaks."
  ([] (periodically-clear-currently-playing-synths! 3000))
  ([timeout-ms]
   (if @clear-loop-running?
     (timbre/info "The `periodically-clear-currently-playing-synths!` loop is already running.")
     (do (reset! clear-loop-running? true)
         (a/go-loop
          []
           (try (clear-currently-playing-synths!)
                (catch Exception e (timbre/error "Error in `periodically-clear-currently-playing-synths!`"
                                                 e)))
           (a/<! (a/timeout timeout-ms))
           (when clear-loop-running?
             (recur)))))))

(defn stop-clear-loop!
  []
  (reset! clear-loop-running? false))

(comment
  (require '[tieminos.overtone-extensions :as oe])
  (oe/defsynth sini
    [freq 200, amp 0.5, gate 1, a 1, d 1, r 1, out 0]
    (o/out out (* amp (o/env-gen (o/envelope [0 1 1 0] [a d r])
                                 :gate gate
                                 :action o/FREE)
                  (o/pan2 (o/sin-osc freq)))))

  (def s1 (sini))

  (def s (with-meta (sini {:a 10 :d 10 :r 10})
           {:live-controls.synth/dur 20}))

  (stop-long-running-synths!*
   10 [s]))
