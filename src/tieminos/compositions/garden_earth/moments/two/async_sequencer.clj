(ns tieminos.compositions.garden-earth.moments.two.async-sequencer
  (:require
   [clojure.core.async :as async]
   [taoensso.timbre :as timbre]))

(def sections
  [{:name :section-1
    :dur/minutes 1/12
    :on-start (fn [] (println "starting S0"))
    :on-end (fn [] (println "ending S0"))}
   {:name :section-2
    :dur/minutes 1/12
    :on-start (fn [] (println "starting S1"))
    :on-end (fn [] (println "ending S1"))}
   {:name :section-3
    :dur/minutes 1/12
    :on-start (fn [] (println "starting S2"))
    :on-end (fn [] (println "ending S2"))}
   {:name :section-4
    :dur/minutes 1/12
    :on-start (fn [] (println "starting S3"))
    :on-end (fn [] (println "ending S3"))}])

(defonce pause-chan (async/chan))
(defonce resume-chan (async/chan))
(defonce skip-chan (async/chan))
(defonce prev-chan (async/chan))
(defonce stop-chan (async/chan))

(defn pause [] (async/>!! pause-chan :pause))   ;; Pause
(defn resume [] (async/>!! resume-chan :resume)) ;; Resume
(defn skip [] (async/>!! skip-chan :skip))     ;; Skip to the next section
(defn prev [] (async/>!! prev-chan :prev))
(defn stop [] (async/>!! stop-chan :prev))

(defn run-sections
  [sections start-at]
  (let [paused? (atom false)]
    (async/go-loop [section-index start-at]
      (if-let [section (nth sections section-index nil)]
        (let [{:keys [dur/minutes on-start on-end]} section
              on-end* (fn []
                       (timbre/info "Stopping section:" (:name section))
                       (on-end))]
          (timbre/info "Starting section:" (:name section))
          (on-start)
          (let [section-timeout (async/timeout (* minutes 60 1000))]
            (async/alt!
              section-timeout
              (if @paused?
                (recur section-index)
                (do
                  (on-end*)
                  (println "Going to next section")
                  (recur (inc section-index))))

              pause-chan
              (do
                (println "Paused" section-index)
                (reset! paused? true)
                (async/<! resume-chan) ;; Wait for resume
                (on-end*)
                (println "Resumed")
                (recur (inc section-index)))

              skip-chan
              (do
                (on-end*)
                (println "Skipping to next section!")
                (recur (inc section-index)))

              prev-chan
              (if (nth sections (dec section-index) nil)
                (do (on-end*)
                    (println "Going back to previous section!")
                    (recur (dec section-index)))
                (do ;; TODO this could be improved, but need to prevent `on-start` from running again
                  (on-end*)
                  (println "Already at the first section!")
                  (recur section-index)))

              stop-chan
              (do (on-end*)
                  (println "Stopping!")
                  (recur (count sections))))))
        (println "All sections completed")))))

(comment
  (let [start-at 0]
    (run-sections sections start-at))

  (pause)
  (resume)
  (skip)
  (prev)
  (stop))
