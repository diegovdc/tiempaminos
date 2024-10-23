(ns tieminos.sc-utils.ndef.v1
  "A basic Ndef implementation"
  (:require
   [overtone.core :as o]))

(defonce ndefs (atom {}))

(defmacro ndef
  [id synth
   & {:keys [out
             fade-time
             fade-in
             fade-out
             group]
      :or {out 0
           fade-time 3
           fade-in fade-time
           fade-out fade-time}}]
  `(let [synth# ~(remove nil?
                         `((o/synth
                            [~(symbol "gate") 1]
                            (o/out ~out
                                   (~(symbol "*") (o/env-gen
                                                   (o/asr ~fade-in 1 ~fade-out)
                                                   :gate ~(symbol "gate")
                                                   :action o/FREE)
                                                  ~synth)))
                           ~group))]

     (when-let [prev-synth# (get @ndefs ~id)]
       (try (o/ctl prev-synth# :gate 0)
            (catch Exception ~(symbol "_e") nil)))
     (swap! ndefs assoc ~id synth#)))

(defn stop
  ([] (when-let [ndefs* (seq (keys @ndefs))]  (apply stop ndefs*)))
  ([& ids]
   (doseq [id ids]
     (when-let [prev-synth (get @ndefs id)]
       (try (o/ctl prev-synth :gate 0)
            (catch Exception _e nil))
       (swap! ndefs dissoc id)))))

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
  (ndef :my-id (* 0.2 (o/sin-osc 100))
        :group [:head my-group]
        :fade-in 0.5
        :fade-out 5)
  (ndef :my-id (* 0.2 (o/sin-osc 100))
        :group [:head my-group]
        :fade-in 5
        :fade-out 0.5)

  (o/stop))
