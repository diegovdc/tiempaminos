(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc
  (:require
   [clojure.math :refer [round]]
   [org.httpkit.client :as http]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.harmonies :refer [meta-pelog]]
   [tieminos.habitat.extended-sections.harmonies.chords :refer [fib-21
                                                                meta-slendro1]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls :as bardo.live-ctl]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :refer [delete-bank-bufs]]
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
                                              (assoc :on? on?
                                                     :start-time (System/currentTimeMillis))))
  (if on?
    (bardo.live-ctl/start-recording {:input-k input})
    (bardo.live-ctl/stop-recording {:input-k input})))

(comment
  (toogle-rec {:input :guitar
               :on? true}))

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

(defn set-active-recorded-bank
  [inputs bank]
  (swap! live-state (fn [state]
                      (reduce (fn [state* input] (assoc-in state* [:rec input :active-bank] bank))
                              state
                              inputs))))

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
         ;; TODO lower extra vol
         (+ 18 (first (linlin 0 1 -32 6 [amp])))))
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

(defn set-active-synth
  [player opt-num]
  (let [synth-key (case opt-num
                    0 :granular
                    1 :crystal
                    (throw (ex-info "Unkown synth" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :active-synth] synth-key)))

(defn set-active-bank
  [{:keys [player bank on?]}]
  (swap! live-state assoc-in [:algo-2.2.9-clouds player :active-banks (dec bank)] on?))

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

(defn set-harmony
  [player opt-num]
  (let [env (case opt-num
              0 :meta-slendro
              1 :fib
              2 :meta-pelog
              (throw (ex-info "Unkown harmony:" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :harmony] env)))

(defn set-harmonic-speed
  [player harmonic-speed]
  (swap! live-state
         assoc-in [:algo-2.2.9-clouds player :harmonic-speed]
         (round (first (linlin 0 1 5 70 [harmonic-speed])))))

(defn- update-harmonic-range
  [hrange low? value]
  (assoc hrange
         (if low? :low :high)
         (round (first (linlin 0 1
                               (if low? -30 30)
                               (if low? 30 -30)
                               [value])))))

(defn set-harmonic-range
  [{:keys [player low? value]}]
  (swap! live-state
         update-in [:algo-2.2.9-clouds player :harmonic-range]
         update-harmonic-range
         low?
         value))

(defn set-rev-send
  [{:keys [player clean? value]}]
  (let [out (if clean? :clean :processes)]
    (swap! live-state
           assoc-in [:algo-2.2.9-clouds player :reaper.send/reverb out]
           value)))

(defn delete-bank
  "`k` is a key for where to find the active bank of the player"
  [inputs]
  (doseq [input-k inputs]
    (let [active-bank (-> @live-state :rec input-k (:active-bank 0))]
      (delete-bank-bufs input-k active-bank))))

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
           "/Milo/bank-rec-radio" (set-active-recorded-bank [:mic-1 :mic-2] (first args))
           "/Milo/bank-delete-btn" (when press? (delete-bank [:mic-1 :mic-2]))
           "/Milo/toggle-bank" (set-active-bank {:player :milo :bank (:index args-map) :on? (== 1 (:on args-map))})
           "/Milo/synth-radio" (set-active-synth :milo (first args))
           "/Milo/harmony-radio" (set-harmony :milo (first args))
           "/Milo/harmonic-speed" (set-harmonic-speed :milo (first args))
           "/Milo/harmonic-lowest-note" (set-harmonic-range {:player :milo :low? true :value (first args)})
           "/Milo/harmonic-highest-note" (set-harmonic-range {:player :milo :low?  false :value (first args)})
           "/Milo/rev-send-clean" (set-rev-send {:player :milo :clean? true :value (first args)})
           "/Milo/rev-send-process" (set-rev-send {:player :milo :clean? false :value (first args)})
           "/Diego/rec-guitar-btn" (toogle-rec {:input :guitar :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
           "/Diego/rec-durs-radio" (switch-rec-durs [:guitar] (first args))
           "/Diego/rec-pulse-radio" (switch-rec-pulse [:guitar] (first args))
           "/Diego/clouds-active-btn" (toggle-clouds :diego press?)
           "/Diego/clouds-amp" (set-clouds-amp :diego (first args))
           "/Diego/clouds-env-radio" (set-clouds-env :diego (first args))
           "/Diego/clouds-rhythm-radio" (set-clouds-rhythm :diego (first args))
           "/Diego/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :diego (first args))
           "/Diego/bank-rec-radio" (set-active-recorded-bank [:guitar] (first args))
           "/Diego/toggle-bank" (set-active-bank {:player :diego :bank (:index args-map) :on? (== 1 (:on args-map))})
           "/Diego/bank-delete-btn" (when press? (delete-bank [:guitar]))
           "/Diego/synth-radio" (set-active-synth :diego (first args))
           "/Diego/harmony-radio" (set-harmony :diego (first args))
           "/Diego/harmonic-speed" (set-harmonic-speed :diego (first args))
           "/Diego/harmonic-lowest-note" (set-harmonic-range {:player :diego :low? true :value (first args)})
           "/Diego/harmonic-highest-note" (set-harmonic-range {:player :diego :low?  false :value (first args)})
           "/Diego/rev-send-clean" (set-rev-send {:player :diego :clean? true :value (first args)})
           "/Diego/rev-send-process" (set-rev-send {:player :diego :clean? false :value (first args)})
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
  (reset! live-state {})
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (throttled-post (dissoc new-value :lorentz))))
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (println new-value))))
