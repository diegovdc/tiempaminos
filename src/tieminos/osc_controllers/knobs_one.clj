(ns tieminos.osc-controllers.knobs-one
  "A 12 knobs template"
  (:require [clojure.edn :as edn]
            [overtone.core :as o]
            [overtone.midi :as midi]
            [re-affect.alpha.core :as ræ]
            [taoensso.timbre :as timbre]
            [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers :refer [osc-bool]]
            [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-router :as osc-router]
            [tieminos.math.utils :refer [linexp* linlin*]]
            [tieminos.osc-controllers.clients :as osc.clients]
            [tieminos.osc-controllers.server :as osc.server]
            [tieminos.osc.reaper :as reaper]
            [tieminos.utils :refer [cb-interpolate]]))

;; TODO: pagination
;; TODO: page names

;;;;;;;;;;;;;;;;;;
;; * Usage
;;;;;;;;;;;;;;;;;;
(comment
  ;; initialize
  (init! {:port 7777 ;; default value
          :clients [["127.0.0.1" 7778] ;; default value
                    #_["192.168.0.101" 7777]]
          :midi-sink (midi/midi-out "Bus 1") ;; default value
          ;; maximum 12 knobs
          :ctls [{:label "T1Vol" ;; If action = `:track-vol`, then defaults to "T<track>Vol"
                  :color :volume ;; "00FF00FF" - can be a keyword or a hex color string (see the `get-color` fn) 
                  :visible? true ;; defaults to true

                  :action {:type :track-vol
                           :dur-ms 5000 ;; interpolation duration, defaults to 5000
                           :mapping-fn identity ;; default - a fn that maps the interpolated incoming knob value (0-1) to another value
                           :track 1}}
                 {:action {:type :track-vol :track 2}
                  :color :volume}
                 {:action {:type :track-vol :track 3}
                  :color :volume}
                 {:action {:type :track-vol :track 4}
                  :color :volume}
                 {:label "MixGain" ;; If action = `:midi-cc`, then defaults to "CC<cc>"
                  :color :surge
                  :action {:type :midi-cc
                           :dur-ms 5000 ;; interpolation duration, defaults to 5000
                           :mapping-fn knob->midi ;; default - a fn that maps the interpolated incoming knob value (0-1) to another value, in thi case to 0-127
                           :cc 16  ;; 16-32 are good MIDI CC numbers
                           :chan 0 ;; defaults to 0
                           :default-val 0.6}}
                 {:label "Freq" ;; If action = `:sc-synth`, then defaults to "SC<param>"
                  :color :sc
                  :action {:type :sc-synth
                           :dur-ms 5000 ;; interpolation duration, defaults to 5000
                           :mapping-fn #(linexp* 0 1 100 1000 %)
                           :param :freq}}
                 {:label "Pan"
                  :color :sc
                  :action {:type :sc-synth
                           :dur-ms 5000 ;; interpolation duration, defaults to 5000
                           :mapping-fn #(linlin* 0 1 -1 1 %)
                           :param :pan}}
                 {:label "Amp" ;; If action = `:sc-synth`, then defaults to ":param"
                  :color :sc
                  :action {:type :sc-synth
                           :dur-ms 100 ;; interpolation duration, defaults to 5000
                           :mapping-fn #(linlin* 0 1 0 0.7 %)
                           :param :amp}}]}))
;;;;;;;;;;;;;;;;;;
;; * API
;;;;;;;;;;;;;;;;;;
(declare dispatch get-subval state get-sc-synth-params-impl)

(defn init!
  "(Re)initialize the OSC controller and all the infrastructure"
  [config]
  (dispatch {::init config}))

(defn get-sc-synth!
  []
  (get-subval ::sc-synth))

(defn set-sc-synth!
  [synth]
  (dispatch {::set-sc-synth synth}))

(defn get-sc-synth-params!
  ([] (get-sc-synth-params! {}))
  ([defaults]
   (merge defaults (get-sc-synth-params-impl @state))))

;;;;;;;;;;;;;;;;;;
;; ** API/Mapping Fns
;;;;;;;;;;;;;;;;;;

(defn linmap [min* max*] #(linlin* 0 1 min* max* %))

(defn expmap [min* max*] #(linexp* 0 1 min* max* %))

(def freqmap #'expmap)

(defn dbampmap [min* max*] #(->> % (linlin* 0 1 min* max*) o/db->amp))

(defn filtermap [min* max*] #(->> % (linexp* 0 1 min* max*) (min 20000) (max 30)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Re-affect/State init
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private initial-state {})

(defonce ^:private state (atom initial-state))

(ræ/reg-state ::db state)
(declare reg-event-db reg-event-fx reg-fx reg-sub)
(ræ/defapi ::db)

;;;;;;;;;;;;;;;;;;
;; * Router
;;;;;;;;;;;;;;;;;;

(def router
  (osc-router/osc-router
   "/knob:index" (dispatch {::on-knob-change
                            {:index (:index path-params)
                             :value (first args)}})))

(defn osc-responder
  [{:keys [path args] :as _msg}]
  (osc-router/match-by-path router path args))

;;;;;;;;;;;;;;;;;;
;; * Events & FX
;;;;;;;;;;;;;;;;;;
(declare get-label get-color)
(reg-event-fx ::init
              (fn [{:keys [db]} {:keys [ctls] :as config}]
                (let [label-messages (mapv (fn [index {:keys [label action]}]
                                             [::send-msg [(str "/label" index) (get-label label action)]])
                                           (range)
                                           ctls)
                      color-messages (mapv (fn [index {:keys [color]}]
                                             [::send-msg [(str "/knob" index "/color") (get-color color)]])
                                           (range)
                                           ctls)
                      visibility-messages (mapv (fn [index]
                                                  (let [ctl (-> ctls (nth index nil))
                                                        visible? (if ctl
                                                                   (:visible? ctl true)
                                                                   false)]
                                                    [::send-msg [(str "/group" index "-visible") (osc-bool visible?)]]))
                                                (range 12))
                      indexed-ctls (mapv (fn [i ctl] (assoc ctl :index i))
                                         (range)
                                         ctls)
                      config* (assoc config :ctls indexed-ctls)]
                  {:db (assoc db :config config*)
                   :fx (concat [[::init config]]
                               label-messages
                               color-messages
                               visibility-messages)})))
(reg-fx ::init
        (fn [{:keys [db]} {:keys [port clients reinit?]
                           :or {port 7777
                                clients [["127.0.0.1" 7778]
                                         ["192.168.0.101" 7777]]}
                           :as _config}]
          (when (or (not (:inited? db))
                    (not= clients (-> db :config :clients))
                    (not= port (-> db :config :port))
                    reinit?)
            (timbre/info "Initializing...")
            (reaper/init)
            (osc.server/init {:port port})
            (osc.clients/make-reaper-osc-client)
            (osc.clients/make-receiver-clients clients)
            (osc.clients/make-internal-osc-client port)
            (osc.server/responder #'osc-responder)
            (dispatch {::confirm-init true}))))

(reg-event-db ::confirm-init (fn [db _] (assoc db :inited? true)))

(reg-fx ::send-msg
        (fn [_ [path & data]]
          (apply osc.clients/send-osc-msg path data)))

(declare get-midi-sink interpolate-midi-cc interpolate-track-vol interpolate-sc-synth)

(reg-event-fx ::on-knob-change
              (fn [{:keys [db]} {:keys [index value]}]

                (let [midi-sink (get-midi-sink db)
                      index*  (edn/read-string index)
                      {:as _ctl-data :keys [action]} (-> db :config :ctls (nth index*))
                      action-type (-> action :type)
                      db* (assoc-in db [:ctls-state index* :current-value] value)]
                  (case action-type
                    :track-vol (let [{:keys [dur-ms track mapping-fn]
                                      :or {dur-ms 5000}} action]
                                 {:db db*
                                  :fx {::interpolate-track-vol {:dur-ms dur-ms
                                                                :track track
                                                                :value value
                                                                :mapping-fn mapping-fn}}})
                    :midi-cc (let [{:keys [dur-ms cc chan default-val mapping-fn]
                                    :or {dur-ms 5000}} action]
                               {:db db*
                                :fx {::interpolate-midi-cc {:dur-ms dur-ms
                                                            :midi-sink midi-sink
                                                            :cc cc
                                                            :chan chan
                                                            :default-val default-val
                                                            :value value
                                                            :mapping-fn mapping-fn}}})
                    :sc-synth {:db db*
                               :fx {::interpolate-sc-synth (assoc action :value value)}}))))

(reg-fx ::interpolate-track-vol
        (fn [_ interpolator-params]
          (interpolate-track-vol interpolator-params)))

(reg-fx ::interpolate-midi-cc
        (fn [_ interpolator-params]
          (interpolate-midi-cc interpolator-params)))

(reg-fx ::interpolate-sc-synth
        (fn [_ interpolator-params]
          (interpolate-sc-synth interpolator-params)))

(reg-event-db ::set-sc-synth
              (fn [db synth]
                (assoc db :sc-synth synth)))

(reg-sub ::sc-synth (fn [{:keys [sc-synth]}] sc-synth))

(comment
  (-> @state))
;;;;;;;;;;;;;;;;;;
;; * Utils
;;;;;;;;;;;;;;;;;;

(defn- get-color
  [x]
  (cond
    (keyword? x) (case x
                   :volume "F4FFA1FF"
                   :surge "FF8600FF"
                   :myth "7E4DFFFF"
                   :plasmonic "00FF00FF"
                   :six-sines "21F2FFFF"
                   :sc "7FFFF0FF"
                   "FF0000FF")
    (string? x) x
    :else "FF0000FF"))

(defn- get-label
  [label action]
  (cond
    label label
    (= :track-vol (:type action)) (format "T%sVol" (:track action))
    (= :midi-cc (:type action)) (format "CC%s" (:cc action))
    (= :sc-synth (:type action)) (str (:param action))
    :else "-"))

(defn- interpolate-track-vol
  [{:keys [dur-ms track-number value mapping-fn]
    :or {mapping-fn identity}}]
  (let [cb (fn [data] (reaper/set-vol track-number (mapping-fn (:val data))))]
    (if (zero? dur-ms)
      (cb {:val value})
      (cb-interpolate {:id (keyword "track-vol" (str track-number))
                       :dur-ms dur-ms
                       :tick-ms 70
                       :init-val reaper/zero-db
                       :target-val value
                       :cb cb}))))

(defn- knob->midi
  "Linear mapping from knob value to a midi value"
  [value]
  (int (linlin* 0 1 0 127 value)))

(defn- interpolate-midi-cc
  [{:keys [dur-ms sink cc chan default-value value mapping-fn]
    :or {mapping-fn knob->midi}}]
  (let [cb (fn [data]
             (midi/midi-control sink cc (mapping-fn (:val data)) chan))]
    (if (zero? dur-ms)
      (cb {:val value})
      (cb-interpolate {:id (keyword "midi-cc" (str cc "-" chan))
                       :dur-ms dur-ms
                       :tick-ms 70
                       :init-val default-value
                       :target-val value
                       :cb cb}))))
(let [default-midi-sink (memoize (fn [] (midi/midi-out "Bus 1")))]
  (defn- get-midi-sink
    [db]
    (-> db :config (:midi-sink (default-midi-sink)))))

(defn- interpolate-sc-synth
  [{:keys [dur-ms mapping-fn param value]
    :or {mapping-fn identity}}]
  (let [id (keyword "sc-synth" (name param))
        synth (get-subval ::sc-synth)
        init-val (-> synth :args (get (name param)))
        cb (fn [data]
             (when (o/node-active? synth)
               (o/ctl synth param (mapping-fn (:val data)))))]
    (cond
      (not init-val) (timbre/warn "Unknown param for :sc-synth knob:" param)
      (not (o/node-active? synth)) nil
      (zero? dur-ms) (cb {:val value})
      :else (cb-interpolate
             {:id id
              :dur-ms dur-ms
              :tick-ms 70
              :init-val init-val
              :target-val value
              :cb cb}))))

(comment
  (o/defsynth sini
    [freq 200
     amp 0.5
     pan 0
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc freq) pan))))

  (set-sc-synth! (sini (get-sc-synth-params!)))

  (get-sc-synth!)
  (o/ctl (get-sc-synth!) :freq 200)
  (o/kill (get-sc-synth!)))

(defn get-sc-synth-params-impl
  [{:keys [config ctls-state] :as _db}]
  (let [sc-synth-ctls (->> config :ctls (filter #(-> % :action :type (= :sc-synth))))]
    (reduce (fn [params {:as _ctl-data
                         :keys [index action]}]
              (let [{:keys [mapping-fn param]} action
                    knob-value (get-in ctls-state [index :current-value])]
                (if knob-value
                  (assoc params param (mapping-fn knob-value))
                  params)))
            {}
            sc-synth-ctls)))

