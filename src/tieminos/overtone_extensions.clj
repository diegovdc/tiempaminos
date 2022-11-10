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

(comment
  (macroexpand-1 '(defsynth hola [x 1 freq 200] (o/out 0 (* 0.2 (o/sin-osc freq)))))
  (defsynth hola [x 1 freq 200] (o/out 0 (* 0.2 (o/sin-osc freq))))
  (hola :freq 500)
  (o/stop))

(defn- circle-az*
  [pan-az-out]
  (let [[a b c d]
        pan-az-out]
    [a b d c]))

(defn circle-az [& {:keys [num-channels in pos level width orientation]
                    :as pan-az-args}]
  (circle-az* (apply o/pan-az (flatten (seq pan-az-args)))))

(comment
  (o/stop)
  (defsynth test-4chan-circle-az
    [freq 200
     dur 20]
    (let [f (mapv #(* % freq) [1 1.2 1.34 1.5])]
      (o/out 0 (* (o/env-gen (o/envelope [0 1 1 0]
                                         (map #(* % dur) [0.2 0.6 0.2]))
                             :action o/FREE)
                  (circle-az :num-channels 4
                             :in (* 0.2 (+ #_(o/white-noise)
                                           (o/mix (o/saw f))))
                             :pos (o/lf-saw 0.2))))))
  (def test (test-4chan-circle-az :dur 40))
  (o/ctl test :freq 800))
