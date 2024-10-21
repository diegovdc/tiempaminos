(ns tieminos.compositions.garden-earth.moments.two.async-sequencer
  (:require
   [clojure.core.async :as async]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.live-state :as two.ls]
   [tieminos.osc.reaper :as reaper]
   [tieminos.utils :refer [now]]))

(def sections
  [{:name :section-1
    :dur/minutes 1/12
    :on-start (fn [] (println "on-start S0"))
    :on-end (fn [] (println "on-end S0"))}
   {:name :section-2
    :dur/minutes 1/12
    :on-start (fn [] (println "on-start S1"))
    :on-end (fn [] (println "on-end S1"))}
   {:name :section-3
    :dur/minutes 1/12
    :on-start (fn [] (println "on-start S2"))
    :on-end (fn [] (println "on-end S2"))}
   {:name :section-4
    :dur/minutes 1/12
    :on-start (fn [] (println "on-start S3"))
    :on-end (fn [] (println "on-end S3"))
    :handlers {:exp/btn-2 {:description "Prints a line that says \"btn-2\""
                           :fn/on (fn [_] (timbre/log "btn-2 handler call"))
                           :fn/off (fn [_] (timbre/log "btn-2 handler call"))}}}])

(defonce pause-chan (async/chan))
(defonce resume-chan (async/chan))
(defonce skip-chan (async/chan))
(defonce prev-chan (async/chan))
(defonce stop-chan (async/chan))

(defn pause [] (when (:piece/running? @two.ls/live-state)
                 (async/>!! pause-chan :pause)))   ;; Pause
(defn resume [] (when (:piece/running? @two.ls/live-state)
                  (async/>!! resume-chan :resume))) ;; Resume
(defn skip [] (when (:piece/running? @two.ls/live-state)
                (async/>!! skip-chan :skip)))     ;; Skip to the next section
(defn prev [] (when (:piece/running? @two.ls/live-state)
                (async/>!! prev-chan :prev)))
(defn stop [] (when (:piece/running? @two.ls/live-state)
                (async/>!! stop-chan :prev)))

(defn- add-countdown-section
  [initial-countdown-seconds sections]
  (if-not initial-countdown-seconds
    sections
    (let [countdown-section {:name :countdown
                             :dur/minutes (/ initial-countdown-seconds 60)
                             :on-start (fn [])
                             :on-end (fn [])}]
      (concat [countdown-section]
              sections))))

;; TODO add :on-section-start and :on-section-end para agregar callbacks que se ejecuten en secciones futuras

(defn run-sections
  [sections start-at
   & {:keys [initial-countdown-seconds
             on-sequencer-start
             on-sequencer-end
             on-sequencer-section-change]
      :or {on-sequencer-start (fn [] (timbre/info "Starting sequencer"))
           on-sequencer-end (fn [] (timbre/info "All sections completed"))
           on-sequencer-section-change (fn [section] (timbre/info "Starting section:" (:name section)))}}]
  (if (:piece/running? @two.ls/live-state)
    (timbre/info "AsyncSequencer already running.")
    (let [sections* (add-countdown-section initial-countdown-seconds sections)
          paused? (atom false)]
      (swap! two.ls/live-state assoc :piece/running? true :piece/start-time (now))
      (on-sequencer-start)
      (async/go-loop [section-index start-at]
        (if-let [section (nth sections* section-index nil)]
          (let [{:keys [dur/minutes on-start on-end handlers]} section
                on-end* (fn []
                          (try
                            (timbre/info "Stopping section:" (:name section))
                            (on-end)
                            (catch Exception e (timbre/error (str "Error on-end section: " (:name section))
                                                             e))))]

            ;; on-sequencer-section-change
            (try (on-sequencer-section-change section)
                 (catch Exception e (timbre/error (str "Error on-sequencer-section-change section: "
                                                       (:name section))
                                                  e)))


            (swap! two.ls/live-state assoc
                   :section (assoc section :start-time (System/currentTimeMillis)))

            ;; on-start
            (try (on-start)
                 (catch Exception e (timbre/error (str "Error on-start section: " (:name section)) e)))

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
                (if (nth sections* (dec section-index) nil)
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
                    (recur (count sections*))))))
          (do
            (try
              (swap! two.ls/live-state assoc :piece/running? false)
              (on-sequencer-end)
              (catch Exception e (timbre/error "Error on-sequencer-end" e)))))))))

(def reaper-events
  {:on-sequencer-start (fn []
                         (reaper/init)
                         (reaper/stop)
                         (reaper/rec))
   :on-sequencer-section-change (fn [section] (reaper/basic-insert-marker (str (:name section))))})

(comment
  (let [start-at 0]
    (run-sections sections start-at
                  reaper-events))

  (pause)
  (resume)
  (skip)
  (prev)
  (stop))



;;;;;;;;;;;;;;;;
;;; async event
;;;;;;;;;;;;;;;;

(defn async-event
  "For small events happening within a sections.
  `:dur-s` and `:wait-s` are expressed in seconds"
  [{:keys [wait-s dur-s on-start on-end]
    :or {dur-s 0}}]
  (async/go
    (try
      (async/<! (async/timeout (* wait-s 1000)))
      (when on-start (on-start))
      (async/<! (async/timeout (* dur-s 1000)))
      (when on-end (on-end))
      (catch Exception e (timbre/error "Error in async-event" e)))))

(comment
  (do (async-event
        {:wait-s 1
         :dur-s 2
         :on-start #(println "Task started!")
         :on-end #(throw (Exception. "Simulated failure"))})  ; Intentional error

      (println "Main thread continues..."))
  (do (async-event
        {:wait-s 1
         :dur-s 2
         :on-start #(println "Task started!" (rand))
         :on-end #(println "Task end!")})  ; Intentional error
      (println "Main thread continues...")))
