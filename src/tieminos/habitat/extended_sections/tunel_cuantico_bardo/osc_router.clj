(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-router
  (:require [reitit.core :as reitit]))
(defn player-name->kw [player-name]
  (case player-name
    "Milo" :milo
    "Diego" :diego))

(defmacro with-router-data
  [body]
  `(fn [{:keys [~'path ~'path-params] :as ~'route-data} ~'args]
     (let [~'player-k (some-> ~'path-params :player player-name->kw)]
       ~body)))

(comment
  (macroexpand-1 '(with-router-data (println path path-params args))))

(defmacro osc-router
  [& routes]
  `(reitit/router
    [~@(map (fn [[path handler]]
              `[~path {:f (with-router-data ~handler)}])
            (partition 2 2 routes))]))

(comment
  (macroexpand-1 '(osc-router
                   "/:player/something/:thingy" (println path path-params player-k args)
                   "/:player/something2/:thingy" (println path path-params player-k args))))
(comment

  (let [router (osc-router
                "/:player/something/:thingy" (println path path-params player-k args)
                "/:player/something2/:thingy" (println path path-params player-k args))
        router-data (reitit/match-by-path router  "/Milo/something/xxx")]
    ((-> router-data :data :f) router-data [1 2 3])))

