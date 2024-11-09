(ns tieminos.compositions.garden-earth.moments.two.live-state
  (:require
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.ajax :refer [post-live-state]]
   [tieminos.utils :refer [now throttle]]))

(defonce live-state (atom {}))

(defonce throttled-post (throttle post-live-state 80))

(defn- remove-section-handler-fns
  [live-state]
  (-> live-state
      (update-in [:section :handlers]
                 #(->> %
                       (map (fn [[k data]] [k (cond-> data
                                                (data :fn/on) (assoc :fn/on true)
                                                (data :fn/off) (assoc :fn/off true))]))
                       (into {})))
      (update :section dissoc :on-start :on-end)))

(defn set-section
  [section]
  (swap! live-state
         assoc
         :section (-> section
                      (assoc :start-time (System/currentTimeMillis))
                      (update :dur/minutes float))))

(comment (remove-section-handler-fns @live-state))

(defn init-watch!
  []
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (let [data (remove-section-handler-fns new-value)]
                 #_(timbre/info data)
                 (throttled-post data)))))

(defn set-piece-start-time!
  []
  (swap! live-state assoc :piece-start-time (now)))
(comment
  (-> @live-state)
  (reset! live-state {})
  (init-watch!)
  (set-piece-start-time!))
