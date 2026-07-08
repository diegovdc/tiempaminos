(ns hooks.make-synth-fn
  (:require [clj-kondo.hooks-api :as api]))

(defn make-synth-fn [{:keys [node]}]
  (let [[_ synth-name _params-map _body _opts] (:children node)
        new-node (api/list-node
                  (list*
                   (api/token-node 'def)
                   (-> synth-name :children first :value  api/token-node)
                   []
                   #_[(api/list-node
                       (list*
                        (api/token-node 'let)
                        (api/vector-node (concat binding-vec
                                                 external-bindings-vec))
                        body))]))]
    {:node new-node}))

(comment
  (-> {:node
       (api/parse-string
        "
(make-synth-fn
         'siny
         (-> {:freq [500 900]
              :amp 1}
             (env1)
             (freq-mixer))
         '(o/out 0
                 (-> (o/sin-osc freq)
                     (:ugen/freq-mixer) 
                     (o/pan2 0)
                     (* amp :ugen/env)))
         {:reset? true})")}

      make-synth-fn))




