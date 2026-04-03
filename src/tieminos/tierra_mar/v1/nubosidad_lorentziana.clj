(ns tieminos.tierra-mar.v1.nubosidad-lorentziana
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [tieminos.sc-utils.groups.v1 :as sc.groups]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(defn init! []
  ;; TODO: implement
  (timbre/warn "Implement"))

;; TODO esto a `init!`
(def lor (lorentz/init-system :x 0.31 :y 0.01 :z 0.02 :dt 0.05))
(def lor2 (lorentz/init-system :x 0.41 :y 0.1 :z 0.082 :dt 0.05))
(def fl-iem-client
  (tm.configs/get-iem-osc-client :nubosidad-lorenztiana-fl))

(def fl2-iem-client
  (tm.configs/get-iem-osc-client :nubosidad-lorenztiana-fl2))

(defn send-lorentzian-flow-osc
  [iem-osc-client lorentz-system i]
  (let [i* (* i)
        az (lorentz/bound (lorentz-system i*) :x -180 180)
        el (lorentz/bound (lorentz-system i*) :y -180 180)
        roll (lorentz/bound (lorentz-system i*) :z -180 180)]
    (osc/osc-send iem-osc-client "/StereoEncoder/azimuth" (float az))
    (osc/osc-send iem-osc-client "/StereoEncoder/elevation" (float el))
    (osc/osc-send iem-osc-client "/StereoEncoder/width" (float (/ (+ az el roll)
                                                                  2)))
    (osc/osc-send iem-osc-client "/StereoEncoder/roll"  (float roll))))

(comment
  (osc/osc-send fl-iem-client "/StereoEncoder/azimuth" (float 5)))
(rain.v2/ref-rain
 :id :nubosidad-lorenztiana-fl
 :durs [0.2]
 :on-event
 (rain.v2/on-event
  (send-lorentzian-flow-osc fl-iem-client lor i)
  (send-lorentzian-flow-osc fl2-iem-client lor2 i)))

(comment
  (rain.v2/stop)
  (o/stop))
;;;;;;;;;;;;;;;;;;
;; Synths
;;;;;;;;;;;;;;;;;;

(defonce testsyn (atom nil))
(defonce testsyn2 (atom nil))
(comment

  (do

    (oe/defsynth nuboso
      [in 0
       out 0]
      (let [sig (->  (o/sound-in in)
                     (o/moog-ladder 700 0.2)
                     (* 8))
            rev (-> sig
                    (o/free-verb 1 1 0.7)
                    (o/pan2 (lfo-kr 1.1 -0.5 1)))]

        (o/out out [(+ sig (first rev))
                    (second rev)])))
    (oe/defsynth nuboso2
      [in 0
       out 0]
      (let [sig (->  (o/sound-in in)

                     (o/b-moog (lfo-kr 1.7 300 500) (lfo-kr 1.2 0.3 0.5) 1)
                     #_(o/lpf 600)
                     (* 0.4))
            rev (-> sig (o/free-verb 1 1.6 0.3)
                    (* (lfo-kr 1.3 0.4 1.5)))
            rev2 (-> rev (o/free-verb 1 1.7 0.8)
                     (o/pitch-shift 0.1 1/2)
                     (* (lfo-kr 1.4 0.4 1.5)))]

        (o/out out [rev rev2])))
    (when @testsyn
      (o/kill @testsyn)
      (reset! testsyn nil))
    (when @testsyn2
      (o/kill @testsyn2)
      (reset! testsyn2 nil))

    (reset! testsyn
            (nuboso
             :in (tm.configs/get-in :fl-main)
             :out (tm.configs/get-out :nubosidades-fl-2ch)))
    (reset! testsyn2
            (nuboso2
             :in (tm.configs/get-in :fl-main)
             :out (tm.configs/get-out :nubosidades-fl2-2ch)))))

;; flujo de señal
;; fl -> nuboso 1 y 2
;; fl -> arp -> nuboso 1 y 2
;;
(comment
  (sc.groups/init-groups!))
