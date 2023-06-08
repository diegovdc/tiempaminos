(ns tieminos.habitat.scratch.async-stuff
  (:require
   [clojure.core.async :as async]
   [taoensso.timbre :as timbre]))

(defonce async-channels (atom {}))

(defn init-async-seq-call-loop!
  []
  (if-not (and (:seq-call-chan @async-channels)
               (:seq-call-loop @async-channels))
    (let [seq-call-chan (async/chan)
          seq-call-loop (async/go-loop
                         []
                          (let [{:keys [timeout f]} (async/<! seq-call-chan)]
                              ;; `when` is used to prevent exceptions when closing the channels
                            (when (fn? f) (f))
                            (when timeout (async/<! (async/timeout timeout)))
                            (recur)))]
      (swap! async-channels assoc
             :seq-call-chan seq-call-chan
             :seq-call-loop seq-call-loop)
      (timbre/info "async-seq-call channels initialized"))
    (throw (ex-info "async-seq-call-loop channels already exist"
                    {:async-channels async-channels}))))

(defn stop-async-seq-call-loop!
  []
  #_(async/close! (:seq-call-loop @async-channels))
  (async/close! (:seq-call-chan @async-channels))
  (swap! async-channels dissoc :seq-call-loop :seq-call-chan)
  (timbre/info "async-seq-call channels stoppped"))

(defn sequence-call
  "Prevent function calls from happening to close together.
  This is useful for example when calling the SC server with synthdefs that might saturate it if too close together and while things are playing."
  [timeout f]
  (if-let [chan (:seq-call-chan @async-channels)]
    (async/>!! chan {:f f :timeout timeout})
    (timbre/error "`init-async-seq-call-loop!' has not been called"
                  {:async-channels async-channels})))

(comment
  (init-async-seq-call-loop!)
  (stop-async-seq-call-loop!)

  (doseq [x (range 5)]
    (sequence-call 1000 #(println x))))
