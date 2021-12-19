(ns tieminos.overtone-extensions
  (:require [overtone.core :as o]))

(defmacro defsynth
  "Like `o/defsynth, but creates a function with destructuring,
  for IDE documentation pruposes.
  It also defines a synth with an \"o\" appended
  at the end that works exactly as the one in `o/defsynth`.
  i.e. for (defsynth hola...) one would get two vars `hola` and `holao`"
  [s-name & s-form]
  {:arglists '([name doc-string? params ugen-form])}
  (let [[s-name params ugen-form] (o/synth-form s-name s-form)
        s-nameo (symbol (str s-name "o"))
        defaults (apply hash-map (first s-form))
        ks (vec (keys defaults))
        args (symbol "args")]
    `(do
       (def ~s-nameo (o/synth ~s-name ~params ~ugen-form))
       (defn ~s-name [& {:keys ~ks :or ~defaults :as ~args}]
         (if (seq ~args) (~s-nameo ~args) (~s-nameo))))))

;; un comentario
(comment
  (macroexpand-1 '(defsynth hola [x 1 freq 200] (o/out 0 (* 0.2 (o/sin-osc freq)))))
  (defsynth hola [x 1 freq 200] (o/out 0 (* 0.2 (o/sin-osc freq))))
  (hola :freq 500 )
  (o/stop))
