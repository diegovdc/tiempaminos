(ns tieminos.sc-utils.synths.template-synth.v0-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [analyze-args
                                                       modify-body
                                                       modify-params
                                                       modify-params2
                                                       qualify-body]]))

(def get-audio-bus (memoize (fn [] (assoc (o/audio-bus) :id 1))))
(let [params {:freq [1 2]
              :levels [0 1 1 0]
              :env-durs [1 1 1]
              :ugen/env '(o/env-gen (o/envelope levels env-durs))
              :shaper-limit 0.5
                ;; :ugen/pan #'panny
              :ugen/rev '(o/sine-shaper shaper-limit)
              :ugen/fx1 '(o/dist)
              :out 0}]
  (modify-body
   params
   (qualify-body params '(o/out 0
                                (-> (o/sin-osc freq)
                                    :ugen/pan
                                    :ugen/rev
                                    :ugen/fx1
                                    :ugen/nilly
                                    (* :ugen/env))))))

(deftest modify-body-test
  (let [params {:freq [1 2]
                :levels [0 1 1 0]
                :env-durs [1 1 1]
                :shaper-limit 0.5
                :out 0
                :ugen/env '(o/env-gen (o/envelope levels env-durs))
                :ugen/rev '(o/sine-shaper shaper-limit)
                :ugen/fx1 '(o/dist)}]
    (testing "Modifies the body of a template to insert the ugens and return a `ugen-form`. If an ugen is missing in the params and has no default (like `:ugen/pan`) or if it is `nil` (like :ugen/nilly`) it it will be substituted by `identity`. Also if the parameters come in a vector they will use serial keys as that is what Overtone expects.")
    ;; NOTE: the actuall function returns a list not a vector, so that `eval` can be called on the resulting body/ugen-form at some point. But for clarity we are using vectors instead of: (list ...) .
    (is (= [#'overtone.core/out
            0
            [#'clojure.core/->
             [#'overtone.core/sin-osc '[freq0 freq1]]
             clojure.core/identity
             '(o/sine-shaper shaper-limit)
             '(o/dist)
             clojure.core/identity
             [#'oc/*
              '(o/env-gen
                (o/envelope
                 [levels0 levels1 levels2 levels3]
                 [env-durs0 env-durs1 env-durs2]))]]]
           (modify-body
            params
            (qualify-body params '(o/out 0
                                         (-> (o/sin-osc freq)
                                             :ugen/pan
                                             :ugen/rev
                                             :ugen/fx1
                                             :ugen/nilly
                                             (* :ugen/env)))))))))

(deftest modify-params-test
  (is (= '[freq0 1
           freq1 2
           levels0 0
           levels1 1
           levels2 1
           levels3 0
           env-durs0 1
           env-durs1 1
           env-durs2 1
           shaper-limit 0.5
           out 0]
         (modify-params {:freq [1 2]
                         :levels [0 1 1 0]
                         :env-durs [1 1 1]
                         :ugen/env '(o/env-gen (o/envelope levels env-durs))
                         :shaper-limit 0.5
                         :ugen/rev '(o/sine-shaper shaper-limit)
                         :ugen/fx1 '(o/dist)
                         :out 0}))))

(deftest modify-params2-test
  (let [merged-params {:in (get-audio-bus)
                       :freq [500 900],
                       :amp 1,
                       :env-levels [0 1 0.1 1 0],
                       :env-durs [1 3 1 1],
                       :ugen/env '(#'overtone.core/env-gen
                                   (#'overtone.core/envelope env-levels env-durs)
                                   :action
                                   2),
                       :ugen/freq-mixer '((#'clojure.core/fn [sig] sig))}
        result (modify-params2 merged-params)]

    (testing "Removes `ugen` keys, and converts vectors into a 'serial' keys"
      (is (= {:amp 1,
              :env-durs0 1,
              :env-durs1 3,
              :env-durs2 1,
              :env-durs3 1,
              :env-levels0 0,
              :env-levels1 1,
              :env-levels2 0.1,
              :env-levels3 1,
              :env-levels4 0,
              :freq0 500,
              :freq1 900
              :in 1}
             result)))
    (testing "Will convert object like `audio-bus`, `buffer` and `sample` to their ids. So that they can be passed into a synth."
      (is (int? (result :in))))
    (testing "If the input is nil or an empty map, return an empty map"
      (is (= {} (modify-params2 {})))
      (is (= {} (modify-params2 nil))))))

(deftest analyze-args-test
  (is (= ["synth-symbol"
          [:freq [:seq 1]]
          [:rev-mix [:number]]
          [:rev-room [:number]]
          [:width [:number]]
          [:outs [:seq 1]]
          [:ugen/mix [:ugen '((fn [%] (if (> (count freq) 1) (o/mix %) %)))]]
          [:levels [:seq 5]]
          [:env-durs [:seq 4]]]
         (analyze-args
          'synth-symbol
          {:freq [500]
           :rev-mix 0.2
           :rev-room 0.5
           :width 1.5
           :outs [0]
           :ugen/mix '((fn [%] (if (> (count freq) 1) (o/mix %) %)))
           :levels [0 1 1 1 0]
           :env-durs [1 5 5 1]}))))
