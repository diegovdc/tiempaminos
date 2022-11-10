(ns hooks.main
  (:require [clj-kondo.hooks-api :as hooks]))

(defn defsynth [{:keys [node]}]
  (let [[synth-name args & body] (rest (:children node))
        new-node (hooks/list-node
                  [(hooks/token-node 'declare)
                   synth-name
                   (hooks/list-node
                    [(hooks/token-node 'fn)
                     (update args :children #(->> % (partition 2 2)
                                                  (map first)))
                     body])])]
    (prn (mapv first args))
    {:node (with-meta new-node (meta node))
     :defined-by 'overtone.core/defsynth}))

#_(defsynth {:node
             (hooks/parse-string
              "(oe/defsynth sini
  [freq 200 amp 0.5 out 0]
  (o/out out (* amp (o/sin-osc freq))))")})

#_(-> (hooks/parse-string
       "(oe/defsynth sini
  [freq 200 amp 0.5 out 0]
  (o/out out (* amp (o/sin-osc freq))))")
      :children
      (nth 2)
      (update :children #(->> % (partition 2 2)
                              (map first))))
