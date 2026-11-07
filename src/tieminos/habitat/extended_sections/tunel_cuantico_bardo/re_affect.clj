(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.re-affect
  (:require [re-affect.alpha.core :as ræ]))

(defonce db (atom {}))
(def state-key :bardo/state)
(ræ/reg-state state-key db)

(ræ/defapi state-key)

(comment
  (get-in @db [:algo-2.2.9-clouds :milo 0 :amp])

  (reg-sub :milo0/amp [:algo-2.2.9-clouds :milo 0 :amp])
  (reg-sub-fx :milo0/amp
              :milo0/amp.subfx
              (fn [_cofx data]
                (println data))))
