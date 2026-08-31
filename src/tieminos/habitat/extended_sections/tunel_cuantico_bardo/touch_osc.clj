(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.touch-osc
  "TouchOSC interface specific stuff"
  (:require
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers :refer [osc-bool
                                                                                send-osc-msg]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.re-affect :as bardo.ræ :refer [reg-event-fx reg-fx reg-sub]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors :as bardo.signal-processor]))

;;;;;;;;;;;;;;;;;;
;; * General
;;;;;;;;;;;;;;;;;;

(reg-event-fx ::toggle-visibility
              (fn [_ {:keys [path visible?]}]
                {:fx {::toggle-visibility {:path path :visible? visible?}}}))

(reg-fx ::toggle-visibility
        (fn [_ {:keys [path visible?]}]
          (send-osc-msg path (osc-bool visible?))))

(reg-fx ::send-msg
        (fn [_ [path & msg]]
          (apply send-osc-msg path msg)))

;;;;;;;;;;;;;;;;;;
;; * Guitar FX
;;;;;;;;;;;;;;;;;;

(defn set-guitar-preset-labels!
  []
  ;; NOTE: presets are hardcoded to 20 buttons
  (doseq [i (range 20)]
    (let [preset (nth bardo.signal-processor/presets-config i nil)

          ;; Because the way touch osc works (non-generative UI)
          ;; For simplicity, the UI was created by alternating btn/label in the document tree.
          ;; Therefore all buttons are odd numbers and labels are even.
          ;; Thus the preset index should be converted from an index into an even number.
          index (* 2 (inc i))]
      (send-osc-msg (str "/presets/guitar/labels/" index) (or (:name preset) "")))))

(comment
  (set-guitar-preset-labels!))

(defn update-guitar-preset-fx-knobs!
  [preset modified-params]
  (let [{:keys [controls default-config]} preset
        ctls-by-param (reduce
                       (fn [m {:keys [param] :as ctl}] (assoc m param ctl)) {} controls)]
    (doseq [i (range 20)]
      ;; Due to impl simplicity reasons in TouchOSC, the document tree has the following mod3 structure knob/param-label/value-label. Succesive controls have this order.
      ;; Therefore the correct indexes need to be mapped to this format (TouchOsc index base is 1))
      (let [{:keys [param name]} (nth controls i nil)
            index (* i 3)
            {:keys [touch-osc/value-label touch-osc/value]} (get modified-params param)
            {:keys [inv-mapping label-mapping]
             :or {label-mapping str}} (get ctls-by-param param)
            default-value (get default-config param)
            touch-osc-default-value (when inv-mapping (inv-mapping default-value))
            ;; TODO: hide controls if param is `nil` (instead of the following)
            ctl-value (float (if-not param 0
                                     (or value touch-osc-default-value 0.5)))
            value-label* (if-not param ""
                                 (or value-label
                                     (str (label-mapping default-value) "*")))]
        (send-osc-msg (str "/guitar-fx-params/control/" (+ 1 index))
                      ctl-value)
        (send-osc-msg (str "/guitar-fx-params/param-label/" (+ 2 index))
                      (or name ""))
        (send-osc-msg (str "/guitar-fx-params/value-label/" (+ 3 index))
                      value-label*)))))

(reg-event-fx
 ::update-guitar-preset-fx-knobs
 (fn [_ data]
   {:fx {::update-guitar-preset-fx-knobs data}}))

(reg-fx
 ::update-guitar-preset-fx-knobs
 (fn [_ {:keys [preset modified-params]}]
   (update-guitar-preset-fx-knobs! preset modified-params)))

(comment
  (require '[tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state])
  (send-osc-msg "/presets/guitar/preset-buttons-visible" (osc-bool true))
  (let [preset (:preset (bardo.live-state/get-processor-active-preset-data!))
        modified-params (bardo.live-state/get-preset-modified-params! preset)]
    (update-guitar-preset-fx-knobs! preset modified-params)))

;;;;;;;;;;;;;;;;;;
;; * CPU
;;;;;;;;;;;;;;;;;;

(reg-event-fx ::on-cpu-usage-data
              (fn [{:keys [db]}
                   {:keys [avg-cpu peak-cpu] :as sc-server-status}]
                (let [usage-str (format "%s/%s" (int avg-cpu) (int peak-cpu))]
                  {:db (assoc db :sc-server-status sc-server-status)
                   :fx [[::send-msg ["/cpu-usage" usage-str]]
                        [::send-msg ["/cpu-usage-box" (cond (> avg-cpu 75) "FF0000FF"
                                                            :else "00000000")]]]})))

(defn avg-cpu-usage-sub
  [{:keys [sc-server-status] :as _db}]
  (:avg-cpu sc-server-status 0))

(reg-sub ::avg-cpu-usage #'avg-cpu-usage-sub)

(comment
  (bardo.ræ/dispatch {::on-cpu-usage-data (overtone.core/server-status)})
  (bardo.ræ/get-subval ::avg-cpu-usage))

(defn init! []
  (set-guitar-preset-labels!))
