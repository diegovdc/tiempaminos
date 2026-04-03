(ns tieminos.tierra-mar.v1.sections)

(defn sections
  "`config-key` is something like `:arp` or `:harmonizer`.
  All configs should be wrapped in a `fn`"
  [config-key live-state-data]
  (let [sections*
        {0 {:arp (fn [] {:subcps-name (wrap-at (:arp/cps-index live-state-data 0) arp-subcps)
                         :interval-seq-fn (partial make-repeat-cell
                                                   (wrap-at (:arp/pattern-fn-index live-state-data 0)
                                                            [[0 2]
                                                             [0 -2]
                                                             [0 3 1 -2]]))})}}]

    #_((get-in sections* [(:section live-state-data 0) config-key]))
    ((get-in sections* [0 :arp]))))
