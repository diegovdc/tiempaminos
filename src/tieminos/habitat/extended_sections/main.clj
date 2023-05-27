(ns tieminos.habitat.extended-sections.main
  "For setting up different recompositions of Habitat.
  i.e. like playing only different sections, and extending them."
  (:require
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.utils :refer [rrange]]))

(comment
  main/context
  main/performance-config
  main/start-sequencer!
  main/stop-sequencer!)

(def intercambios-de-energia
  {:context
   (assoc main/context
          :intercambios-de-energia/min-width 2
          :intercambios-de-energia/input-pairs-fn (fn [{:keys [inputs texto-sonoro-rand-mixer-bus]}]
                                                    (let [ts {:bus @texto-sonoro-rand-mixer-bus}]
                                                      [[(:guitar @inputs) ts]
                                                       [(:guitar @inputs) (:mic-1 @inputs)]
                                                       #_[(:guitar @inputs) (:mic-3 @inputs)]
                                                       [ts (:mic-1 @inputs)]
                                                       #_[(:mic-2 @inputs) ts]
                                                       #_[(:mic-3 @inputs) ts]
                                                       #_[(:mic-4 @inputs) ts]]))
          :intercambios-de-energia/convolver-in2-amp 0.3
          :intercambios-de-energia/convolver-max-amp-fn #(rrange 0.2 0.5))
   :sections [[[0 0] (fn [_])]
              [[0 2] #'amanecer/intercambios-de-energia]
              [[10 25] (fn [_] (println "end"))]] #_(main/quick-sections 5 sections)})

(comment
  (main/start-sequencer! intercambios-de-energia))

(def polinizadores-nocturnos
  {:context (merge main/context {})
   :sections [[[52 22] #'noche/polinizadores-nocturnos]
              [[62 10] (fn [_] (println "end"))]]
   :initial-sections #'noche/polinizadores-nocturnos})

(comment
  (main/start-sequencer! polinizadores-nocturnos))

(comment
  (-> @hseq/context)
  (do (when @habitat-initialized? (main/stop-sequencer! hseq/context))
      (init!)))
