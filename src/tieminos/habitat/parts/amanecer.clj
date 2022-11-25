(ns tieminos.habitat.parts.amanecer
  (:require
   [tieminos.habitat.panners :refer [panner panner-rate]]
   [tieminos.habitat.routing :refer [inputs preouts]]))

(defn init-section-1 [inputs base-preouts]
  (doseq [[k {:keys [bus]}] inputs]
    (let [out (:bus (k base-preouts))
          config {:in bus
                  :type (rand-nth [:clockwise :counter-clockwise :rand])
                  :out out}]
      (when-not (or out bus)
        (throw (ex-info "Can not init section 1, data panner data missing"
                        config)))
      (panner config)
      (panner-rate {:in bus :rate (* 0.15 (rand))}))))

(comment
  (init-section-1 inputs @preouts))
