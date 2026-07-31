(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.touch-osc
  "TouchOSC interface specific stuff"
  (:require
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers :refer [osc-bool
                                                                                send-osc-msg]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors :as bardo.signal-processor]))

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

(defn toggle-processor-preset-buttons-view!
  [{:keys [visible?]}]
  (send-osc-msg "/presets/guitar/preset-buttons-visible" (osc-bool visible?)))

(comment
  (send-osc-msg "/presets/guitar/preset-buttons-visible" (osc-bool true))
  (let [preset (:preset (bardo.live-state/get-processor-active-preset-data!))
        modified-params (bardo.live-state/get-preset-modified-params! preset)]
    (update-guitar-preset-fx-knobs! preset modified-params)))

(comment
  (require '[tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]))
(defn init! []
  (set-guitar-preset-labels!))
