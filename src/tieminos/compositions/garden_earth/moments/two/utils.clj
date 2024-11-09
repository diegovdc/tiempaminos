(ns tieminos.compositions.garden-earth.moments.two.utils)

(defn normalize-rates
  "Ensure `rates` is a vector"
  [rates]
  (if (number? rates) [rates] rates))
