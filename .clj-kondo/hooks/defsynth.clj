(ns hooks.defsynth
  (:require [clj-kondo.hooks-api :as api]))

(defn defsynth [{:keys [node]}]
  (let [[_ synth-name binding-vec & body] (:children node)
        new-node (api/list-node
                  (list*
                   (api/token-node 'def)
                   synth-name
                   [(api/list-node
                     (list*
                      (api/token-node 'let)
                      binding-vec
                      body))]))]
    {:node new-node}))

(comment
  (-> {:node (api/parse-string
              "(o/defsynth rev [in 0 out 0 mix 0.5 room 0.8 damp 0.5 amp 1]
  (o/out out (* amp (o/free-verb (o/in in) mix room damp))))")}
      defsynth))
