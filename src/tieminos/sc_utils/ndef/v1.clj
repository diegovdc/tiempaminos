(ns tieminos.sc-utils.ndef.v1
  "A basic Ndef implementation"
  (:require
   [overtone.core :as o]))

(defonce ndefs (atom {}))

(defmacro ndef
  [id synth & {:keys [out fade-time group]
               :or {out 0 fade-time 3}}]
  `(let [synth# ~(remove nil?
                         `((o/synth
                               [~(symbol "gate") 1]
                               (o/out ~out
                                      (~(symbol "*") (o/env-gen
                                                       (o/asr ~fade-time 1 ~fade-time)
                                                       :gate ~(symbol "gate")
                                                       :action o/FREE)
                                       ~synth)))
                           ~group))]

     (when-let [prev-synth# (get @ndefs ~id)]
       (try (o/ctl prev-synth# :gate 0)
            (catch Exception ~(symbol "_e") nil)))
     (swap! ndefs assoc ~id synth#)))

(defn stop [& ids]
  (doseq [id ids]
    (when-let [prev-synth (get @ndefs id)]
      (try (o/ctl prev-synth :gate 0)
           (catch Exception _e nil))
      (swap! ndefs dissoc id))))

(comment
  (def my-group (o/group "my-group"))

  (do
    (reset! ndefs {})
    (macroexpand-1
      '(ndef :my-id (* 0.2 (o/sin-osc 200)) {:out 0
                                             :group my-group})))

  (ndef :my-id (* 0.2 (o/sin-osc 100))
    :group [:head my-group]
    :fade-time 5)
  (stop :my-id)

  (o/stop))
