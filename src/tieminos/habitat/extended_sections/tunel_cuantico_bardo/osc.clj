(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc
  (:require
   [clojure.math :refer [round]]
   [clojure.set :as set]
   [clojure.string :as str]
   [org.httpkit.client :as http]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls :as bardo.live-ctl]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.presets :as bardo.presets]
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

(defn mute-input
  [input-k mute?]
  (let [track (case input-k
                :mic-1 4
                :mic-2 5)]
    (osc/osc-send @habitat-osc/reaper-client
                  (format "/track/%s/mute" track)
                  (int mute?))))

(comment
  (mute-input :mic-1 1))

(defn switch-rec-durs [inputs dur]
  (let [dur* (case dur
               0 0.5
               1 1
               2 1.5
               3 2.5
               4 4
               5 10
               6 15
               7 20
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
   :amp 0.7
   :reaper.send/reverb {:clean 0 :processes 0}
   :active-banks #{}
   :harmonic-speed 30
   :harmony :m-slendro
   :harmonic-range {:low -18 :high 18}})

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
         (first (linlin 0 1 -36 36 [amp]))))

(comment
  (set-clouds-amp :diego 1))

(defn set-clouds-sample-lib-size
  [player opt-num]
  (let [env (case opt-num
              0 ##Inf
              1 1
              2 2
              3 3
              4 5
              5 8
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
  (swap! live-state update-in [:algo-2.2.9-clouds player :active-banks]
         (if on? set/union set/difference)
         #{(dec bank)}))

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
           value))

  (condp = [player clean?]
    [:diego true] (osc/osc-send @habitat-osc/reaper-client "/track/14/send/1/volume" (float value))
    [:diego false] (osc/osc-send @habitat-osc/reaper-client "/track/15/send/1/volume" (float value))
    [:milo true] (osc/osc-send @habitat-osc/reaper-client "/track/17/send/1/volume" (float value))
    [:milo false] (osc/osc-send @habitat-osc/reaper-client "/track/18/send/1/volume" (float value))))

(defn delete-bank
  "`k` is a key for where to find the active bank of the player"
  [inputs]
  (doseq [input-k inputs]
    (let [active-bank (-> @live-state :rec input-k (:active-bank 0))]
      (delete-bank-bufs input-k active-bank))))

(defn delete-all-banks
  [inputs]
  (doseq [bank (range 8)
          input-k inputs]
    (delete-bank-bufs input-k bank)))

(def default-gusano-config
  {:section 0
   :amp 0.5})

(defn toggle-gusano
  [on?]
  (swap! live-state
         assoc :gusano
         (-> default-gusano-config
             (merge (:gusano @live-state))
             (assoc :on? on?)))
  (if on?
    (bardo.live-ctl/start-gusano)
    (bardo.live-ctl/stop-gusano)))

(defn toggle-gusano-active-sources
  [src on?]
  (swap! live-state update-in [:gusano :sources]
         (fnil (if on? set/union set/difference) #{})
         #{src}))

(comment
  (reset! live-state {})

  (toggle-gusano-active-sources :milo true))

;; TODO set the resulting values of gusano in the live-state just as with the other values
(defn set-gusano-rates
  [i]
  (swap! live-state assoc-in [:gusano :rates] i))

(defn set-gusano-rates-seq-speed
  [i]
  (swap! live-state assoc-in [:gusano :rates-seq-speed] i))

(defn set-gusano-amp
  [amp]
  (swap! live-state assoc-in [:gusano :amp] (first (linlin 0 1 0 1.5 [amp]))))

(defn set-gusano-period
  [i]
  (swap! live-state assoc-in [:gusano :period] i))

(defn set-gusano-durs
  [i]
  (swap! live-state assoc-in [:gusano :durs] i))

(defn set-gusano-grain-trig
  [x]
  (swap! live-state assoc-in [:gusano :grain-trig-rate] x))

(defn set-gusano-grain-dur
  [x]
  (swap! live-state assoc-in [:gusano :grain-dur] x))

(defn set-gusano-2nd-voice
  [x]
  (swap! live-state assoc-in [:gusano :second-voice-index] x))

(comment
  (require '[tieminos.network-utils :refer [get-local-host]])
  (def touchosc-fb-client (osc/osc-client (get-local-host) 16181)))

(defn HACK-parse-path
  "Fixes a problem with the bank button which share the same address, so on feedback they all turn on or off."
  [path]
  (if-not (str/includes? path "/toggle-bank/")
    path
    (->> (str/split path #"/")
         (drop-last 1)
         (str/join "/"))))

(def ^:private excluded-paths #{"/presets/load"})

(defn init!
  "`clients` is a vector of [host port]"
  [clients]
  (habitat-osc/init)
  (habitat-osc/make-reaper-osc-client)
  (habitat-osc/make-receiver-clients clients)
  (let [internal-client (habitat-osc/make-internal-osc-client)]
    (habitat-osc/responder
     (fn [{:keys [path args] :as msg}]
       (let [HACKED-path (HACK-parse-path path) ;; FIXME there should be a more elegant way to handle this (see fn definition).
             args-map (habitat-osc/args->map args)
             press? (= 1.0 (first args))]
         (case HACKED-path
           "/Milo/rec-mic-1-btn" (toogle-rec {:input :mic-1 :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
           "/Milo/rec-mic-2-btn" (toogle-rec {:input :mic-2 :on? press? :dur (-> @live-state :rec :mic-2 :dur (or 0.5))})
           "/Milo/mute-mic-1"    (mute-input :mic-1 (first args))
           "/Milo/mute-mic-2"    (mute-input :mic-2 (first args))
           "/Milo/rec-durs-radio" (switch-rec-durs [:mic-1 :mic-2] (first args))
           "/Milo/rec-pulse-radio" (switch-rec-pulse [:mic-1 :mic-2] (first args))
           "/Milo/clouds-active-btn" (toggle-clouds :milo press?)
           "/Milo/clouds-amp" (set-clouds-amp :milo (first args))
           "/Milo/clouds-env-radio" (set-clouds-env :milo (first args))
           "/Milo/clouds-rhythm-radio" (set-clouds-rhythm :milo (first args))
           "/Milo/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :milo (first args))
           "/Milo/bank-rec-radio" (set-active-recorded-bank [:mic-1 :mic-2] (first args))
           "/Milo/bank-delete-btn" (when press? (delete-bank [:mic-1 :mic-2]))
           "/Milo/bank-delete-all-btn" (when press? (delete-all-banks [:mic-1 :mic-2]))
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
           "/Diego/bank-delete-all-btn" (when press? (delete-all-banks [:guitar]))
           "/Diego/synth-radio" (set-active-synth :diego (first args))
           "/Diego/harmony-radio" (set-harmony :diego (first args))
           "/Diego/harmonic-speed" (set-harmonic-speed :diego (first args))
           "/Diego/harmonic-lowest-note" (set-harmonic-range {:player :diego :low? true :value (first args)})
           "/Diego/harmonic-highest-note" (set-harmonic-range {:player :diego :low?  false :value (first args)})
           "/Diego/rev-send-clean" (set-rev-send {:player :diego :clean? true :value (first args)})
           "/Diego/rev-send-process" (set-rev-send {:player :diego :clean? false :value (first args)})
           ;; gusano
           "/gusano/gusano-active-btn" (toggle-gusano press?)
           "/gusano/gusano-active-milo-src-btn" (toggle-gusano-active-sources :milo press?)
           "/gusano/gusano-active-diego-src-btn" (toggle-gusano-active-sources :diego press?)
           "/gusano/rates" (set-gusano-rates (first args))
           "/gusano/rates-seq-speed" (set-gusano-rates-seq-speed (first args))
           "/gusano/amp" (set-gusano-amp (first args))
           "/gusano/period" (set-gusano-period (first args))
           "/gusano/durs" (set-gusano-durs (first args))
           "/gusano/grain-trig" (set-gusano-grain-trig (first args))
           "/gusano/grain-durs" (set-gusano-grain-dur (first args))
           "/gusano/2nd-voice" (set-gusano-2nd-voice (first args))
           ;; presets
           "/save-preset" (when press? (bardo.presets/save-preset!))
           "/presets/load" (bardo.presets/load-preset! internal-client @habitat-osc/receiver-clients (first args))
           (timbre/warn "Unknown path for message: " msg args-map))

         ;; Save last update to touch-osc-state
         (swap! bardo.live-state/touch-osc-state assoc path args)

         ;; send update to other clients
         (doseq [client (map second @habitat-osc/receiver-clients)]
           (when-not (excluded-paths path)
             (apply osc/osc-send client path args))))))))

(defn ping []
  (http/get "http://localhost:5000/ping"
            (fn [{:keys [status headers body error]}] ;; asynchronous response handling
              (if error
                (println "Failed, exception is " error)
                (println "Async HTTP POST: " status)))))

(comment
  (-> @live-state)
  (get-local-host)
  (init! [["127.0.0.1" 16181]
          #_["192.168.0.100" 16181]
          ["192.168.0.102" 16180]])
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
