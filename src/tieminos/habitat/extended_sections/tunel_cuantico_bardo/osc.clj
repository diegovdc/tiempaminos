(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc
  (:require
   [org.httpkit.client :as http]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls :as bardo.live-ctl]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.utils :refer [throttle]]))

(def default-rec-config
  {:on? true
   :pulse :dur
   :dur 0.5})

(defn toogle-rec [{:keys [input on?]}]
  (swap! live-state assoc-in [:rec input] (-> default-rec-config
                                              (merge (-> @live-state :rec input))
                                              (assoc :on? on?)))
  (if on?
    (bardo.live-ctl/start-recording {:input-k input})
    (bardo.live-ctl/stop-recording {:input-k input})))

(defn switch-rec-durs [inputs dur]
  (let [dur* (case dur
               0 0.5
               1 1
               2 1.5
               3 2.5
               4 4
               5 10
               (throw (ex-info "Unkown rec dur" {:dur dur})))]
    (swap! live-state (fn [state]
                        (reduce (fn [state* input] (assoc-in state* [:rec input :dur] dur*))
                                state
                                inputs))))
  ;; we don't restart the recorder, but the recorder should receive a durs function instead that will deref the live-state somehow
  )

(defn switch-rec-pulse [inputs pulse]
  (let [pulse (case pulse
                0 :dur
                1 :dur*2
                2 :rand-2
                3 :rand-4
                (throw (ex-info "Unkown rec pulse" {:pulse pulse})))]
    (swap! live-state (fn [state]
                        (reduce (fn [state* input] (assoc-in state* [:rec input :pulse] pulse))
                                state
                                inputs))))
  ;; we don't restart the recorder, but the recorder should receive a durs function instead that will deref the live-state somehow
  )

(def default-cloud-config
  {:sample-lib-size 1
   :env :lor-1_4
   :rhythm :lor-0.1_2
   :amp 0.7})

(defn toggle-clouds
  [player on?]
  (swap! live-state
         assoc-in [:algo-2.2.9-clouds  player]
         (-> default-cloud-config
             (merge (-> @live-state :algo-2.2.9-clouds player))
             (assoc :on? on?)))
  (if on?
    (bardo.live-ctl/start-clouds player)
    (bardo.live-ctl/stop-clouds player)))

(comment
  (toggle-clouds :milo true))

(defn set-clouds-amp
  [player amp]
  (swap! live-state
         assoc-in [:algo-2.2.9-clouds player :amp]
         (+ 24 (first (linlin 0 1 -32 6 [amp])))))

(comment
  (set-clouds-amp :diego 1))

(defn set-clouds-sample-lib-size
  [player opt-num]
  (let [env (case opt-num
              0 1
              1 2
              2 3
              3 5
              4 8
              (throw (ex-info "Unkown clouds sample-lib-size" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :sample-lib-size] env)))

(defn set-clouds-env
  [player opt-num]
  (let [env (case opt-num
              0 :lor-1_4
              1 :lor-0.1_2
              2 :a-0.1_0.4*d-2*r-3
              3 :weights-largos
              (throw (ex-info "Unkown clouds env" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :env] env)))

(defn set-clouds-rhythm
  [player opt-num]
  (let [env (case opt-num
              0 :lor-0.1_2
              1 :lor-2_6
              2 :rand-0_10
              3 :rit
              4 :accel
              (throw (ex-info "Unkown clouds rhythm" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :rhythm] env)

    (when (-> @live-state :algo-2.2.9-clouds player :on?)
      (timbre/info "Restarting player-clouds" :player))))

(do
  (defn init! []
    (habitat-osc/init)
    (habitat-osc/responder
     (fn [{:keys [path args] :as msg}]
       (let [args-map (habitat-osc/args->map args)
             press? (= 1.0 (first args))]
         (case path
           "/Milo/rec-mic-1-btn" (toogle-rec {:input :mic-1 :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
           "/Milo/rec-mic-2-btn" (toogle-rec {:input :mic-2 :on? press? :dur (-> @live-state :rec :mic-2 :dur (or 0.5))})
           "/Milo/rec-durs-radio" (switch-rec-durs [:mic-1 :mic-2] (first args))
           "/Milo/rec-pulse-radio" (switch-rec-pulse [:mic-1 :mic-2] (first args))
           "/Milo/clouds-active-btn" (toggle-clouds :milo press?)
           "/Milo/clouds-amp" (set-clouds-amp :milo (first args))
           "/Milo/clouds-env-radio" (set-clouds-env :milo (first args))
           "/Milo/clouds-rhythm-radio" (set-clouds-rhythm :milo (first args))
           "/Milo/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :milo (first args))
           "/Diego/rec-guitar-btn" (toogle-rec {:input :guitar :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
           "/Diego/rec-durs-radio" (switch-rec-durs [:guitar] (first args))
           "/Diego/rec-pulse-radio" (switch-rec-pulse [:guitar] (first args))
           "/Diego/clouds-active-btn" (toggle-clouds :diego press?)
           "/Diego/clouds-amp" (set-clouds-amp :diego (first args))
           "/Diego/clouds-env-radio" (set-clouds-env :diego (first args))
           "/Diego/clouds-rhythm-radio" (set-clouds-rhythm :diego (first args))
           "/Diego/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :diego (first args))
           (timbre/warn "Unknown path for message: " msg args-map))))))

  (init!))

(defn ping []
  (http/get "http://localhost:5000/ping"
            (fn [{:keys [status headers body error]}] ;; asynchronous response handling
              (if error
                (println "Failed, exception is " error)
                (println "Async HTTP POST: " status)))))

(comment
  (init!)
  (ping))

(defn post [endpoint body & {:keys [debug?]}]
  (http/post (str "http://localhost:5000" endpoint)
             {:body (pr-str body)}
             (fn [{:keys [status headers body error]}] ;; asynchronous response handling
               (if error
                 (println "Failed, exception is " error endpoint)
                 (when debug? (println "Async HTTP POST: " status))))))

(def throttled-post
  (throttle (fn [state] (post "/gusano-cuantico-bardo" state))
            50))
(comment
  (->> @live-state)
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (throttled-post (dissoc new-value
                                       :lorentz)))))
