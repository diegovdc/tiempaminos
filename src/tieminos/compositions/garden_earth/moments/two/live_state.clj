(ns tieminos.compositions.garden-earth.moments.two.live-state
  (:require
   [tieminos.compositions.garden-earth.moments.two.ajax :refer [post-live-state]]
   [tieminos.utils :refer [now throttle]]))

(defonce live-state (atom {}))

(defonce throttled-post (throttle post-live-state 80))

(defn init-watch!
  []
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (println new-value)
               (throttled-post new-value))))

(defn set-piece-start-time!
  []
  (swap! live-state assoc :piece-start-time (now)))

(comment
  (reset! live-state {})
  (init-watch!)
  (set-piece-start-time!))
