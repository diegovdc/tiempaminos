(ns tieminos.compositions.garden-earth.moments.two.live-state)

(defonce live-state (atom {}))


(defn init-watch!
  []
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (println new-value)
               #_(post-live-state (-> new-value
                                      (update :arp/pattern :name))))))
