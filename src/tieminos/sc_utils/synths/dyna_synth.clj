(ns tieminos.sc-utils.synths.dyna-synth
  (:require
   [clojure.walk :as walk]
   [overtone.core :as o]
   [overtone.libs.counters :refer [next-id]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

;; Create synths dynamically that can have different arities for envelopes and other parameters inputed as vectors etc.
;;
;; This may just be hack, but it seems to work, it doesn't even create new synths AFAIK

(defmacro synthdef
  [name params body]
  (let [args [{:keys (into [] (keys params))
               :or params}]]
    `(defn ~name
       ([] (~name {}))
       (~args ((o/synth ~body))))))

(defn maybe-mix
  "Useful when passing things that may or may not do mutlichan expansion"
  ;; I think there may be an issue with the overtone impl of `mix`
  ;; as something like ((o/synth (o/out 0 (-> (o/sin-osc [400 500]) (o/pan2 0)))))
  ;; will kind of blow up if mix is not used
  [& params]
  (let [mix-set (->> params
                     (map (fn [param] (try (if (> (count param) 1)
                                             :mix
                                             1)
                                           (catch Exception _ 1))))
                     set)]
    (if (mix-set :mix)
      o/mix
      identity)))

(comment
  (synthdef sini
            {freq (rand-nth [100 200 [300 500]])
             levels [0 1 0]
             env-durs [0.1 0.1]
             pan 0}
            (o/out 0 (-> (o/sin-osc freq)
                         (o/pan2 pan)
                         ((maybe-mix freq pan))
                         (* (o/env-gen (o/envelope levels env-durs)
                                       :action o/FREE)))))

  ;; uso
  (:synth ;; tests that no new synths are created
   (sini {:freq [400 500]
          :levels [0 1  0]
          :env-durs [1 0.1]
          :pan 0}))

  (ref-rain
   :id :test
   :durs [2 3 2]
   :ratio 1/20
   :on-event (on-event
              (let [len 7 #_(inc (rand-int 7))
                    levels (into [] (concat [0] (mapv (fn [_] (rand)) (range 5))  [0]))
                    env-durs (mapv (fn [_] (* 2 (rand))) (range (dec (count levels))))
                    range-start (inc (rand-int 50))
                    range-len (inc (rand-int 20))
                    harmonics (map #(* 100/8 (inc (rand-int 5)) %) (range range-start (+ range-start range-len)))
                    freqs (mapv (fn [_] (rand-nth harmonics)) (range len))]
                #_(println levels env-durs)
                #_(xs [600 1300 300 1200 400 500 300] [0 0.9373508104141479 0.5608688080926528 0.39056312279472527 0.1717293778171871 0.8470538580570205 0]
                      [1.7555834480342927 1.0127793979535171 0.5467674993274259 1.734972579640932 0.4453855926088839 1.5843675344556778]
                      (dec (* 2 (rand))))
                #_(println (:synth (xs freqs levels env-durs (dec (* 2 (rand))))))
                #_(sini 100 200)))))

(comment
  ;; WIP
  (defonce dyna-synths (atom {}))
  (defmacro synthdef
    [name params body]
    (let [args [{:keys (into [] (keys params))
                 :or params}]
          default-args (into {} (map (fn [[k v]] [(keyword k) v]) params))
          call-args (gensym)
          merged-args (gensym)
          analyzed-args (gensym)
          synth (gensym)
          cached-synth (gensym)
          qbody body]
      `(defn ~name
         ([] (~name {}))
         ([{:keys ~(into [] (keys params))
            :or ~params
            :as ~call-args}]
          (let [~merged-args (merge ~default-args ~call-args)
                ~analyzed-args (analyze-ds-args ~merged-args)
                ~cached-synth (get @dyna-synths ~analyzed-args)
                ;; ~(gensym) (println #_~qbody "======" #_ ~merged-args "CCCCCCCC" ~call-args ~merged-args)
                ~synth (if ~cached-synth
                         ~cached-synth
                         (eval `(make-synth ~~merged-args '~~qbody)))]
            #_(println (boolean ~cached-synth) ~analyzed-args #_(eval ~synth))
            (swap! dyna-synths assoc ~analyzed-args ~synth)
            (~synth (modify-params2 ~merged-args)))))))
  #_(reset! dyna-synths {})
  (synthdef sini {freq [200 300] amp 1}
            '(o/out 0 (-> (o/sin-osc freq)
                          (o/mix)
                          (o/pan2 0)
                          (* (o/env-gen (o/env-perc) :action o/FREE)))))

  (sini {:freq [390 800 1500]})

  (defn modify-body [body]
    (println "yyyyyyyyyyyyyy" (first body))
    (def body body)
    (let [res (eval (walk/postwalk (fn [x]
                                     (cond
                                       (= :env-durs x) [1 1]
                                       (= :freq x) 'freq
                                       (= :levels x) [0 1 0]
                                       :else  x))
                                   body))]
      (println res)
      res))

  (do (defn modify-params [params]
        (->> params
             (map (fn [[k v]]
                    (cond
                      (number? v) [(symbol (name k)) v]
                      (vector? v) (map-indexed (fn [i v*]
                                                 [(symbol (str (name k) i)) v*])
                                               v))))
             flatten
             (into [])))
      (defn modify-params2 [params]
        (->> params
             (map (fn [[k v]]
                    (cond
                      (number? v) {k v}
                      (vector? v) (map-indexed (fn [i v*]
                                                 {(keyword (str (name k) i)) v*})
                                               v))))
             flatten
             (apply merge)))
      (modify-params2 {:freq [200 500]
                       :pan 0}))
  (do (defn modify-body [params synth-body]
        (let [params-map (->> params
                              (map (fn [[k v]]
                                     (cond
                                       (number? v) [(symbol k) (symbol k)]
                                       (vector? v) [(symbol k) (into [] (map-indexed (fn [i v*]
                                                                                       (symbol (str (name k) i)))
                                                                                     v))])))
                              (into {}))]

          (walk/postwalk (fn [x]
                           (if-let [mapping (params-map x)]
                             mapping x))
                         synth-body)))

      (modify-body {:freq [200 500]
                    :pan 0}
                   '(o/out 0 (-> (o/sin-osc freq)
                                 (* (o/env-gen (o/env-perc) :action o/FREE))))))

  (defmacro make-synth [params-map synth-body]
    #_(println [params-map synth-body])
    (let [[s-name params ugen-form]
          (o/synth-form 's-name [(modify-params params-map) (eval (modify-body params-map synth-body))])]
      `(o/synth ~s-name ~params ~ugen-form)))
  ((make-synth {:freq [200 300]} '(o/out 0 (-> (o/sin-osc freq)
                                               #_(o/mix)
                                               (* (o/env-gen (o/env-perc) :action o/FREE)))))
   :freq0 500
   :freq1 800)

  (defn make-synthf [params-map synth-body]
    (make-synth params-map synth-body)))
