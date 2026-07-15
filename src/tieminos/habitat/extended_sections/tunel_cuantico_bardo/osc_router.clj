(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-router
  (:require
   [reitit.core :as reitit]
   [taoensso.timbre :as timbre]))

(defn player-name->kw [player-name]
  (case player-name
    "Milo" :milo
    "Diego" :diego
    (throw (ex-info (str "Unknown player: " player-name)  {:player-name player-name}))))

(defmacro with-router-data
  [body]
  `(fn [{:keys [~'path ~'path-params] :as ~'route-data} ~'args]
     (let [~'player-k (some-> ~'path-params :player player-name->kw)]
       ~body)))

(comment
  (macroexpand-1 '(with-router-data (println path path-params args))))

(defmacro osc-router
  "Creates a `reitit/router` from pairs of `path` and `handler-body`.

  The handler body is a form wrapped into a function.

  It gets the following arguments passed in:
  `path`, `path-params`, `route-data` (all the data including the previous two)
  and `player-k` (when there is a `:player` `path-param`)."
  [& routes]

  (when-not (even? (count routes))
    (throw (ex-info "[osc-router] takes an even number of arguments. It expects pairs of params: path and handler-body" {})))
  `(reitit/router
    [~@(map (fn [[path handler]]
              `[~path {:handler-fn (with-router-data ~handler)}])
            (partition 2 2 routes))]))

(comment
  (macroexpand-1 '(osc-router
                   "/:player/something/:thingy" (println path path-params player-k args)
                   "/:player/something2/:thingy" (println path path-params player-k args))))

(defn match-by-path [router path args]
  (try
    (let [router-data (reitit/match-by-path router path)]
      (if-let [f (-> router-data :data :handler-fn)]
        (f router-data args)
        (timbre/warn "No handler-fn:" {:path path  :router-data router-data})))
    (catch Exception e
      (timbre/warn "Unknown path:" {:path path :args args :ex-message (.getMessage e)}))))

(comment
  (let [router (osc-router
                "/:player/something/:thingy" (println path path-params player-k args)
                "/:player/something2/:thingy" (println path path-params player-k args))
        router-data (reitit/match-by-path router  "/Milos/something/xxx")]
    ((-> router-data :data :handler-fn) router-data [1 2 3]))
  (let [router (osc-router
                "/:player/something/:thingy" (println path path-params player-k args)
                "/:player/something2/:thingy" (println path path-params player-k args))
        path "/Milos/something/xxx"
        args [1 2 3]]
    (match-by-path router  path  args)))


