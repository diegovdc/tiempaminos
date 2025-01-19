(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
  (:require [tieminos.attractors.lorentz :as lorentz]))

(defonce touch-osc-state (atom {}))
(defonce live-state (atom {:lorentz (lorentz/init-system :x 0.3 :y 0.02 :z 0.012)}))

(defn init-watch!
  [id f]
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (f new-value))))

(defn get-active-banks
  [player-k]
  (->> @live-state :algo-2.2.9-clouds player-k :active-banks))

(comment
  (->> @live-state))
