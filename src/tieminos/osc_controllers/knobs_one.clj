(ns tieminos.osc-controllers.knobs-one
  "A 12 knobs template"
  (:require [clojure.edn :as edn]
            [overtone.midi :as midi]
            [re-affect.alpha.core :as ræ]
            [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers :refer [osc-bool]]
            [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-router :as osc-router]
            [tieminos.math.utils :refer [linlin*]]
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
                    ["192.168.0.101" 7777]]
          :midi-sink (midi/midi-out "Bus 1") ;; default value
          ;; maximum 12 knobs
          :ctls [{:label "T1Vol" ;; If action = `:track-vol`, then defaults to "T<track>Vol"
                  :color :volume ;; "00FF00FF" - can be a keyword or a hex color string (see the `get-color` fn) 
                  :visible? true ;; defaults to true
                  :action {:type :track-vol
                           :dur-ms 5000 ;; interpolation duration, defaults to 5000
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
                           :cc 16       ;; 16-32 are good MIDI CC numbers
                           :chan 0      ;; defaults to 0
                           :default-val 0.6}}]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Re-affect/State init
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private initial-state {})

(defonce ^:private state (atom initial-state))

(ræ/reg-state ::db state)
(declare reg-event-db dispatch get-subval reg-event-fx reg-fx reg-sub)
(ræ/defapi ::db)

(defn init!
  "(Re)initialize the OSC controller and all the infrastructure"
  [config]
  (dispatch {::init config}))

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
                                                (range 12))]
                  {:db (assoc db :config config)
                   :fx (concat [[::init config]]
                               label-messages
                               color-messages
                               visibility-messages)})))

(reg-fx ::init
        (fn [_ {:keys [port clients]
                :or {port 7777
                     clients [["127.0.0.1" 7778]
                              ["192.168.0.101" 7777]]}
                :as _config}]
          (reaper/init)
          (osc.server/init {:port port})
          (osc.clients/make-reaper-osc-client)
          (osc.clients/make-receiver-clients clients)
          (osc.clients/make-internal-osc-client port)
          (osc.server/responder #'osc-responder)))

(reg-fx ::send-msg
        (fn [_ [path & data]]
          (apply osc.clients/send-osc-msg path data)))

(declare get-midi-sink interpolate-midi-cc interpolate-track-vol)

(reg-event-fx ::on-knob-change
              (fn [{:keys [db]} {:keys [index value]}]
                (let [midi-sink (get-midi-sink db)
                      ctl-data (-> db :config :ctls (nth (edn/read-string index)))]
                  (case (-> ctl-data :action :type)
                    :track-vol (let [{:keys [dur-ms track]
                                      :or {dur-ms 5000}} (:action ctl-data)]
                                 (interpolate-track-vol dur-ms track value))
                    :midi-cc (let [{:keys [dur-ms cc chan default-val]
                                    :or {dur-ms 5000}} (:action ctl-data)]
                               (interpolate-midi-cc dur-ms midi-sink cc chan default-val value))))))

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
                   "FF0000FF")
    (string? x) x
    :else "FF0000FF"))

(defn- get-label
  [label action]
  (cond
    label label
    (= :track-vol (:type action)) (format "T%sVol" (:track action))
    (= :midi-cc (:type action)) (format "CC%s" (:cc action))
    :else "-"))

(defn- interpolate-track-vol
  [dur-ms track-number value]
  (let [cb (fn [data] (reaper/set-vol track-number (:val data)))]
    (if (zero? dur-ms)
      (cb {:val value})
      (cb-interpolate {:id (keyword "track-vol" (str track-number))
                       :dur-ms dur-ms
                       :tick-ms 70
                       :init-val reaper/zero-db
                       :target-val value
                       :cb (fn [data] (reaper/set-vol track-number (:val data)))}))))

(defn- knob->midi
  "Linear mapping from knob value to a midi value"
  [value]
  (int (linlin* 0 1 0 127 value)))

(defn- interpolate-midi-cc
  [dur-ms sink cc chan default-value value]
  (let [cb (fn [data]
             (midi/midi-control sink cc (knob->midi (:val data)) chan))]
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
