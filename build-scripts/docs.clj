(ns docs
  (:require
   [codox.main :as codox]
   [taoensso.timbre :as timbre]))

(defn run-codox-with-timeout
  "Generate the documentation.
  Because Overtone causes the program to not exit, then we manually shutdown."
  [{:keys [timeout-ms codox-args]}]
  (timbre/info "================= Initiating Codox Generation =======================")
  (let [f (future (codox/generate-docs codox-args))]
    (timbre/info "================= Setting timeout =======================")
    (Thread/sleep timeout-ms)
    (timbre/info "================= Timeout done =======================" (future-done? f))
    (when (not (future-done? f))
      (future-cancel f))
    (timbre/info "Codox generation timed out")
    (System/exit 0)))
