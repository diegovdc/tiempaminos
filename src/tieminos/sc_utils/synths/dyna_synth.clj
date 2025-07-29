(ns tieminos.sc-utils.synths.dyna-synth
  (:require
   [clojure.walk :as walk]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
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

(defn partition-chans
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
    (println mix-set)
    (if (mix-set :mix)
      o/mix
      identity)))

(partition-chans [1 2 3]
                 [1 3 4])

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

(defn ns-kw?
  [kw-ns-str kw]
  (and (keyword? kw) (= kw-ns-str (namespace kw))))
(-> #'my-pan)

(defn analyze-arg
  [k arg]
  (cond
    (number? arg) [:number]
    (sequential? arg) [:seq (count arg)]
    (ns-kw? "fx" k) [:fx/fn (str arg)]
    (ns-kw? "dyn" k) [:dyn/fn (str arg)]
    :else (throw (ex-info "Don't know how to analyze arg:" {:key k
                                                            :arg arg}))))

(defn analyze-ds-args [m]
  (->> m
       (mapv (fn [[k arg]]
               [k (analyze-arg k arg)]))))
  ;; WIP

(defonce dyna-synths (atom {}))

(defn modify-params [params]
  (println "MP" params)
  (->> params
       (keep (fn [[k v]]
               (cond
                 (number? v) [(symbol (name k)) v]
                 (vector? v) (map-indexed (fn [i v*]
                                            [(symbol (str (name k) i)) v*])
                                          v))))
       flatten
       (into [])))
(modify-params {:freq [390 100 2000], :pan 0, :amp 1, :fx/pan identity})
(defn modify-params2 [params]
  (println "MP2" params)
  (->> params
       (mapv (fn [[k v]]
               (cond
                 (number? v) {k v}
                 (vector? v) (map-indexed (fn [i v*]
                                            {(keyword (str (name k) i)) v*})
                                          v))))
       flatten
       (apply merge)))
(modify-params2 {:freq [200 500]
                 :pan 0})

(do (defn modify-body [params synth-body]
      (println "MB" params synth-body)
      (let [params-map (->> params
                            (map (fn [[k v]]
                                   (cond
                                     (number? v) [(symbol k) (symbol k)]
                                     (vector? v) [(symbol k) (into [] (map-indexed (fn [i v*]
                                                                                     (symbol (str (name k) i)))
                                                                                   v))])))
                            (into {}))]

        (timbre/spy :info (walk/postwalk (fn [x]
                                           (if-let [mapping (params-map x)]
                                             mapping
                                             (cond
                                               (ns-kw? "fx" x) (params x identity)
                                               (ns-kw? "dyn" x) (list 'as-> 'sig (params x identity))
                                               :else x)))
                                         synth-body))))

    #_(modify-body {:freq [200 500]
                    :pan 0
                    :fx/pan (fn [sig] (o/pan-az 4 sig (o/line 0 0)))}
                   '(o/out 0 (-> (o/sin-osc freq)
                                 :fx/pan
                                 (* (o/env-gen (o/env-perc) :action o/FREE)))))
    (modify-body {:freq [200 500]
                  :pan 0
                  :dyn/pan '(o/pan-az 4 sig (o/line 0 0))}
                 '(o/out 0 (-> (o/sin-osc freq)
                               :dyn/pan
                               (* (o/env-gen (o/env-perc) :action o/FREE))))))

(-> 1
    (as-> sig (+ 1 sig)))

(defn partition-chans
  [sig n-chans]
  (println sig)
  (if (sequential? sig)
    (partition n-chans n-chans nil sig)
    sig))

(defmacro make-synth [params-map synth-body]
  (let [[s-name# params ugen-form]
        (let [body (eval (modify-body params-map synth-body))]
          (println "==============" body)
          (o/synth-form 's-name [(modify-params params-map) body]))]
    `(o/synth ~s-name# ~params ~ugen-form)))

(comment

  ((make-synth {:freq [200 200]} '(o/out 0 (-> (o/sin-osc freq)
                                               #_(partition-chans 2)

                                               #_(o/mix)
                                               (* (o/env-gen (o/env-perc) :action o/FREE))
                                               (o/pan2)
                                               (o/mix))))
   :freq0 100
   :freq1 400
   :freq2 150)
  (macroexpand-1 '(make-synth {:freq [200 300 600]
                               :fx/pan may-pan}
                              '(o/out 0 (-> (o/sin-osc freq)
                                            (partition-chans 2)
                                            :fx/pan
                                            (o/mix)
                                            (o/pan2 0)
                                            (* (o/env-gen (o/env-perc) :action o/FREE)))))))
(name :fx/hola)
(defn parse-params
  [m]
  (->> m
       (map (fn [[k v]]
              (cond
                (ns-kw? "fx" k) [(symbol (str "fx__" (name k))) v]
                (ns-kw? "dyn" k) [(symbol (str "dyn__" (name k))) v]
                :else [k v])))
       (into {})))
(do
  (defn get-cached-synth
    [analyzed-args]
    (println "888888888888888" analyzed-args)
    (def analyzed-args analyzed-args)
    (get-in @dyna-synths [analyzed-args :synth]))
  (defmacro synthdef
    [name params body]
    (let [default-args (into {} (map (fn [[k v]] [(keyword k) v]) params))
          call-args (gensym)
          merged-args (gensym)
          analyzed-args (gensym)
          synth (gensym)
          cached-synth (gensym)
          parsed-params (parse-params params)]
      (println params)
      (println parsed-params)
      `(defn ~name
         ([] (~name {}))
         ([{:keys ~(into [] (keys parsed-params))
            :or ~parsed-params
            :as ~call-args}]
          (let [~merged-args (merge ~default-args ~call-args)
                ~analyzed-args (into ['~name] (analyze-ds-args ~merged-args))
                ~cached-synth  (get-cached-synth ~analyzed-args)
                ;; ~(gensym) (println #_~qbody "======" #_ ~merged-args "CCCCCCCC" ~call-args ~merged-args)
                ~(gensym)  (println "--------" ~merged-args)
                ~synth (if ~cached-synth
                         ~cached-synth
                         (eval `(make-synth ~~merged-args '~~body)))]
            #_(println (boolean ~cached-synth) ~analyzed-args #_(eval ~synth))
            (println "====================!!!!!!!!!!!!" ~analyzed-args)
            (println "@@@@@@@" ~cached-synth)
            (when-not ~cached-synth
              (println "swaping")
              (swap! dyna-synths assoc ~analyzed-args
                     {:synth ~synth
                      :form ~body}))
            (~synth (modify-params2 ~merged-args))))))))

(comment
  (-> analyzed-args)
  (get-cached-synth analyzed-args)
  (let [i 0]
    (= (nth (-> analyzed-args) i)
       (nth ['sini
             [:freq [:seq 3]]
             [:pan [:number]]
             [:amp [:number]]
             [:fx/pan [:fx/fn my-pan]]]
            i)))
  (-> @dyna-synths)
  (get-in @dyna-synths
          [['sini
            [:freq [:seq 3]]
            [:pan [:number]]
            [:amp [:number]]
            [:fx/pan [:fx/fn my-pan]]]
           :synth])
  (sini)
  (do
    ;; `:fx` kw test
    (reset! dyna-synths {})
    (synthdef sini
              {freq [200 300]
               pan 0
               amp 1
         ;; TODO: (WIP) add support for args like {:fx/pan my-pan}: `modify-body` already supports this, but the `params` of the `synthdef` doesn't, they need to be replaced somehow.
         ;; Still missing is being able to pass in parameters to the `:fx` functions.
               :fx/pan my-pan}

              '(o/out 0 (-> (o/sin-osc)
                            :fx/pan
                            (* (o/env-gen (o/env-perc) :action o/FREE)))))
    (let [x {:freq [390 100 2000]}]
      (sini x)))
  (do
    ;; `:dyn` kw test
    (reset! dyna-synths {})
    (synthdef sini
              {freq [200 300]
               pan 0
               amp 1
         ;; TODO: (WIP) add support for args like {:fx/pan my-pan}: `modify-body` already supports this, but the `params` of the `synthdef` doesn't, they need to be replaced somehow.
         ;; Still missing is being able to pass in parameters to the `:fx` functions.
               :dyn/pan '(o/pan-az 4 sig (o/line 0 2))}

              '(o/out 0 (-> (o/sin-osc)
                            :dyn/pan
                            (* (o/env-gen (o/env-perc) :action o/FREE)))))
    (let [x {:freq [390 100 2000]}]
      (sini x)))

  (-> @dyna-synths
      (get [[:freq [:seq 3]]
            [:pan [:number]]
            [:amp [:number]]
            [:fx/pan [:fx/fn my-pan]]])
      :form)
  (macroexpand-1
   '(synthdef sini
              {freq [200 300]
               amp 1
               :fx/pan #(o/pan2 % 0)}
              '(o/out 0 (-> (o/sin-osc freq)
                            (o/mix)
                            :fx/pan
                            (* (o/env-gen (o/env-perc) :action o/FREE)))))))
(comment

  (defn my-pan [sig] (o/pan-az 4 sig (o/line 0 2)))
  (do
    (reset! dyna-synths {})
    (synthdef sini
              {freq [200 300]
               pan 0
               amp 1
               :fx/pan my-pan  ;; #(o/pan2 % 0)
               ;; TODO: add support for args like {:fx/pan my-pan}: `modify-body` already supports this, but the `params` of the `synthdef` doesn't, they need to be replaced somehow.
               }
              '(o/out 0 (-> (o/sin-osc)
                            :fx/pan
                            (* (o/env-gen (o/env-perc) :action o/FREE)))))
    (let [x {:freq [390 100 2000]}]
      (sini x)))

  ((o/synth (o/out 0 (-> (o/sin-osc)
                         my-pan
                         (* (o/env-gen (o/env-perc) :action o/FREE)))))))

(comment
  (-> @dyna-synths
      keys)
  (reset! dyna-synths {})
  ;; refrain test
  (synthdef sini3
            {freq [200 300]
             amp 1}
            '(o/out 0 (-> (o/sin-osc freq)
                          (* (o/env-gen (o/env-perc) :action o/FREE)))))

  (sini3 {:freq [100 200]})
  (gp/stop)
  (ref-rain
   :id :prueba
   :durs [1]
   :on-event (on-event
              (sini3 {:freq
                      (at-i [[390 100]
                             [390 100 2000]
                             [100 200 300 400 500 600 700]])}))))
