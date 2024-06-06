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

(defn circle-az
  "4-channel circular az panning.
  Using `:orientation` `0` as default is a good idea:
  that way single channels will lie at:
  `-1`: back right
  `-0.5`: back left
  `0`: front left
  `0.5`: front right

  A minimum `:width` of around `1.3` is good for panning from single channel to single channel.
  Maximum `:width` should be `4` otherwise artifacts may be produced."
  [& {:keys [_num-channels _in _pos _level _width _orientation]
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

(defn- circle-az-8ch*
  [pan-az-out]
  pan-az-out
  #_(let [[a b c d e f g h]
          pan-az-out]
      [a b d f h g e c]))

(defn circle-az-8ch
  "8-channel circular az panning.
  Using `:orientation` `0` as default is a good idea:
  that way single channels will lie at:
  `-1`: back right
  `-0.5`: back left
  `0`: front left
  `0.5`: front right

  A minimum `:width` of around `1.3` is good for panning from single channel to single channel.
  Maximum `:width` should be `4` otherwise artifacts may be produced."
  [& {:keys [_num-channels _in _pos _level _width _orientation]
      :as pan-az-args}]
  (circle-az-8ch* (apply o/pan-az (flatten (seq (assoc pan-az-args :num-channels 8))))))

(comment
  (require '[tieminos.habitat.routing :refer [reaper-returns] :as routing])
  (o/stop)
  (defsynth test-8chan-circle-az
    [freq 200
     dur 20]
    (o/out (routing/get-percussion-processes-main-out)
           (* 0.2
              (o/pan-az :num-channels 8
                        :in (o/lpf (o/saw freq) 2000)
                        :pos (o/mouse-x:kr 0 2)
                        :width 2))))
  (def test (test-8chan-circle-az :dur 10 :freq (* (rand-nth [1 2 3 4]) 200)))
  (o/stop)
  (defsynth sini2
    [out 1]
    (o/out out [(* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))
                (* 0.2 (o/sin-osc))]))
  (sini2 :out
         (reaper-returns 6))
  (defsynth balance-test [out 0]
    (o/out out (* 0.5 (o/pink-noise))))
  (def b (balance-test))
  (o/ctl b :out 1)
  (o/stop)
  (o/ctl test :freq 800))
