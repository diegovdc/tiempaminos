(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events
  (:require
   [clojure.core.async :as a]
   [taoensso.timbre :as timbre]))

;;;;;;;;;;;;;
;; Async
;;;;;;;;;;;;;

(defonce chans (atom {}))

(defonce coms-active? (atom true))

(defonce debug-events?
  ;; "Prevent calls to event-handler, and instead print event data"
  (atom false))

(defn init-async-coms!
  [event-handler]
  (if-not (seq @chans)
    (do
      (timbre/info "Starting async event handling!")
      (reset! coms-active? true)
      (let [main (a/chan)]
        (a/go-loop
         []
          (let [event (a/<! main)]
            (try (if-not @debug-events?
                   (event-handler event)
                   (timbre/info "Debugging:\n" event))
                 (catch Exception e (timbre/error "Live Controls async error" e "\n" event))))
          (if @coms-active?
            (recur)
            (do
              (reset! chans {})
              (timbre/info "Stopping async event handling..."))))
        (reset! chans {:main main})))
    (timbre/info "Async event handling already running.")))

(defn dispatch*
  "Dispatches an event."
  [chan-k event]
  (if-let [c (chan-k @chans)]
    (a/put! c event)
    (timbre/error (format "There is no channel \"%s\" to dispatch event. Main need to call `init-async-coms!` first."
                          chan-k))))

(defn dispatch
  "Dispatches an event on the `:main` events channel."
  [event]
  (dispatch* :main event))

(comment
  (reset! debug-events? true)
  (reset! debug-events? false)
  (a/put! (:main @chans) {:hola "mundo"})
  (dispatch {:type :echo :data {:hola "mundo"}})
  (dispatch {:type :halt!})
  (init-async-coms! tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls/event-handler))
