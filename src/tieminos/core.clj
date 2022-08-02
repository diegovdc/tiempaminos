(ns tieminos.core
  (:require
   [clojure.string :as str]
   [time-time.dynacan.players.gen-poly :as gp]
   [overtone.core :as o]
   [overtone.sc.machinery.server.connection :as oc]
   [potemkin :refer [import-vars]]
   [taoensso.timbre :as timbre]))

(defn init-garden-earth! []
  (in-ns 'erv-fib-synth.compositions.garden-earth.core))

(def stop gp/stop)

(defn connect [] (o/connect-external-server))
(defn test-sound [] (o/demo (* 0.2 (o/sin-osc))))

(defn disconnect [] (oc/shutdown-server))

(comment
  (connect)
  (timbre/set-level! :debug)
  (timbre/set-level! :info))

(def warn-on-debug
  (memoize
   (fn [ns-name]
     (timbre/warn "Trying to log a debug statement an external ns, but debugging external files is disabled. See timbre config.  ns: " ns-name))))

(defn print-debug?
  [level-name ns-name]
  (cond  (and (= level-name "debug")
              (str/includes?  ns-name "erv-fib-synth")) true
         (= level-name "debug") (warn-on-debug ns-name)
         :else true))

(timbre/merge-config!
 {:output-fn
  #_timbre/default-output-fn
  (fn [data]
    (let [{:keys [level ?err #_vargs msg_ ?ns-str ?file #_hostname_
                  timestamp_ ?line]} data]
      (when (print-debug? (name level) ?ns-str)
        (str
         (-> (force timestamp_) (str/split #" ") second) " "
         (str/upper-case (first (name level)))  " "
         "[" (or ?ns-str ?file "?") ":" (or ?line "?") "] - "
         (force msg_)
         (when-let [err ?err]
           (str "\n" (timbre/stacktrace err)))))))})

(defn ap-synth [synth-var arg-map]
  (let [ks (->> (meta synth-var) :arglists first (map keyword))
        synth (var-get synth-var)
        defaults (->> synth meta :params
                      (map (fn [{:keys [name default]}] [(keyword name) default]))
                      (into {}))
        vs (map #(get arg-map % (defaults %)) ks)]
    (apply synth vs)))

(comment
  (connect 2)
  (test-sound)
  (disconnect))
(comment
  (def scale
    (->> [13 7 11 9 5]
         (cps/->cps 2)
         cps/set->maps
         (cps/bound-ratio 2)
         (cps/maps->data :bounded-ratio)
         :scale

         #_(#(cps/filter-scale % #{7}))
         #_user/spy
         #_(#(demo! %
                    :periods 1
                    :base-freq 320
                    :note-dur 300
                    :direction :up))))
  (-> scale)

  (def fib-scale [1.1459102934487975
                  1.2360828548001543
                  1.3262554161515112
                  1.3819820590666498
                  1.4721546204180067
                  1.5278812633331453
                  1.583607906248284
                  1.6180469715698396
                  1.7082195329211964
                  1.763946175836335
                  1.8196728187514737
                  1.8541118840730293
                  1.909838526988168
                  1.9442775923097235
                  1.9787166576312791
                  2.0000000000000004])

  (do
    (o/defsynth sqrsin [freq 358 pitch 5, a 1 s 1 r 1 pan 0 amp 0.3]
      (o/out 0 (-> (o/sin-osc freq)
                   (* #_(o/env-gen (o/envelope
                                    [0 0.1 0.1 0]
                                    [a s r]
                                    :lin)
                                   :action o/FREE)

                      (o/square (* 0.5 pitch))
                      amp)
                   (o/pan2 pan)))))

  (comment
    (def s (sqrsin (* 358) 6 0.001 0.02 0.11)))

  #_(o/stop)
  (def v (atom {:freq 722
                :pitch 8
                :a 5
                :s 2}))

  (comment
    (def s2 (sqrsin (* 358 2) 6 0.001 0.02 0.11))

    (reset! gp/refrains {}))

  (comment
    (ref-rain :id ::a
              :durs [3/7 1/7 4/7 1/6 1/15 1/2]
              :tempo 90
              :on-event (on-event (case (wrap-at index [1 0 1 4 0 0  3 3 4])
                                    1
                                    (o/ctl s :freq (wrap-at index [800 305 700 600 100 800 900 10 1111]))
                                    0
                                    (o/ctl s :freq (wrap-at index [70 75 110]))
                                    3 (o/ctl s :freq (wrap-at index [358
                                                                     (* 2 358)
                                                                     (* 3 358)]))
                                    4 (o/ctl s :freq (wrap-at index [(* 3 358)
                                                                     (* 14/3 358)
                                                                     (* 17/3 358)
                                                                     (* 14/3 358)
                                                                     (* 1/3 358)
                                                                     (* 10/3 358)])))))

    (ref-rain :id ::b11
              :ref ::a
              :durs [1/10 1/7]
              :ratio 1/4
              :on-event (on-event (cond (wrap-at index [1 0 0 1 1 0])
                                        1
                                        (o/ctl s
                                               :pitch (wrap-at index [3 4 7])
                                               :amp (wrap-at index [0.2 0.4]))
                                        0
                                        (o/ctl s
                                               :pitch (wrap-at index [5 9 16])
                                               :a (wrap-at index [0.5 0.7 1 2]))
                                        nil)))

    (ref-rain :id ::a2
              :ref ::a
              :durs [1 1/5 1 1/5 1 1/7 2]
              :ratio 1/5
              :on-event (on-event  (case (wrap-at index [1 0 1])
                                     1
                                     (o/ctl s2 :freq (wrap-at index [100 455 110 400]))

                                     0
                                     (o/ctl s2 :freq (wrap-at index [100 20 100 399])
                                            :amp  (wrap-at index [0.1 0.2 0.5])
                                            :a (wrap-at index [2 1])))))
    (ref-rain :id ::a5
              :ref ::a
              :durs [1/3 1/2 1/3 1/2 1/2 1]
              :ratio 1/3
              :on-event (on-event  (case (wrap-at index [1 0 2 0 3])
                                     1
                                     (o/ctl s2 :pitch (wrap-at index [4 1 6 7]))
                                     0
                                     (o/ctl s2 :freq (wrap-at index [60 100 90 300]))
                                     2
                                     (o/ctl s2 :pitch (wrap-at index [2 7 2 6 80]))
                                     3
                                     (o/ctl s2
                                            :freq (wrap-at index [300 100 1500])
                                            :amp (wrap-at index [0.4 0.3 0.1]))
                                     nil)))

    (ref-rain :id ::c
              :ref ::b
              :durs2 [1/5]
              :ratio 2
              :on-event (on-event (swap! v as2s2oc
                                         :pitch (+ 5 (* 100 (wrap-at (* 2 index) fib-s2cale))))))

    (o/stop)))
