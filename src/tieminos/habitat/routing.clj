(ns tieminos.habitat.routing
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe :refer [defsynth]]
   [tieminos.habitat.groups :as groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [ctl-synth ctl-synth2]]))

;; TODO IMPORTANT free the buses with `(when bus (o/free-bus bus))`

(comment
  ;;  USAGE
  (defsynth input
    [in 0 out 0]
    (o/out out (o/sound-in in)))
  (def guitar (input (:guitar ins) 0))
  (o/ctl guitar :out (reaper-returns 2))
  (o/stop))

(def return-n-chans (atom 4))

#_(def reaper-returns ;; previous version
    "Returns map, numbered after the returns in reaper i.e. 1 based numbers"
    (let [starting-chan 33 ;; previous channels belong to the outputs of the instruments from reaper that go into SC
          n-chans @n-chans
          total-returns 6
          return-outs (range starting-chan
                             (+ starting-chan
                                (* n-chans total-returns))
                             n-chans)]
      (println n-chans)
      (into {} (map-indexed (fn [i out] [(inc i) out]) return-outs))))

(defn reaper-returns
  [return]
  (let [starting-chan 33 ;; previous channels belong to the outputs of the instruments from reaper that go into SC
        n-chans @return-n-chans]
    (+ starting-chan (* n-chans (dec return)))))

(comment
  (-> reaper-returns)
  (reset! return-n-chans 8)
  (reaper-returns 5))

;; TODO eventually remove
(def clean-return (reaper-returns 1))
(def reverb-return (reaper-returns 2))
(def processes-return-1
  "Return for buffers and sounds processed by SC"
  (reaper-returns 3))

;; NOTE ns will need to be recompiled if the `return-n-chans` value changes. Therefore it's better to not use these `def`s  anymore.
(def guitar-main-out (reaper-returns 1))
(def guitar-processes-main-out (reaper-returns 2))
(def percussion-main-out (reaper-returns 3))
(def percussion-processes-main-out (reaper-returns 4))
(def mixed-main-out (reaper-returns 5))
(def non-recordable-main-out (reaper-returns 6))
(def main-returns
  {:guitar guitar-main-out
   :guitar-processes guitar-processes-main-out
   :percussion percussion-main-out
   :percussion-processes percussion-processes-main-out
   :mixed mixed-main-out
   :non-recordable non-recordable-main-out})

(defn get-guitar-main-out [] (reaper-returns 1))
(defn get-guitar-processes-main-out [] (reaper-returns 2))
(defn get-percussion-main-out [] (reaper-returns 3))
(defn get-percussion-processes-main-out [] (reaper-returns 4))
(defn get-mixed-main-out [] (reaper-returns 5))
(defn get-non-recordable-main-out [] (reaper-returns 6))

(oe/defsynth recordable-output ;; TODO make octophonic version
  ;; mixdown a 4-ch input into a 1-ch output
  [bus 0 out 0]
  (o/out out (o/mix (o/in bus 4))))

(defn make-recorable-outputs
  [output-buses]
  (->> (dissoc output-buses :non-recordable)
       (mapv (fn [[k bus-num]]
               (let [bus (o/audio-bus 1 (str (name k) "-recordable-output-bus"))]
                 [k {:bus bus
                     :synth (recordable-output {:group (groups/preouts :tail)
                                                :bus bus-num
                                                :out bus})}])))
       (into {})))
;; should be 1-channel signals
(defonce recordable-outputs (atom {}))

(defn init-recordable-inputs!
  [output-buses]
  (reset! recordable-outputs (make-recorable-outputs output-buses)))

(comment
  (init-recordable-inputs! main-returns)
  (o/stop)
  (def s ((o/synth (o/out (reaper-returns 1)
                          (* 0.2 (o/lpf (o/saw 300) 1500))))
          (groups/early))))
;;;;;;;;;;;;;;;;;;;;;;
;; inputs and input buses ;;
;;;;;;;;;;;;;;;;;;;;;
(defsynth input
  [in 0 out 0]
  (o/out out (o/sound-in in)))

(comment
  (-> inputs)
  (def test-bus (o/audio-bus 1 "test-bus"))
  (-> test-bus)
  (def s ((o/synth (o/out test-bus
                          (* 0.2 (o/lpf (o/saw 300) 1500))))))
  (input {:group (groups/early)
          :in (:in (:texto-sonoro inputs))
          :out (:bus (:texto-sonoro inputs))})
  (o/stop))

(def ins
  "Input name to blackhole channels map"
  {:guitar 20
   :mic-1  21
   :mic-2  22
   :mic-3  23
   :mic-4  24
   :mic-5  25
   :mic-6  26
   :mic-7  27
   :texto-sonoro 28})

(defonce inputs (atom {}))
(defonce special-inputs (atom {}))

(def diego-ins #{"guitar-bus"})
(def milo-ins #{"mic-1-bus" "mic-2-bus" "mic-3-bus" "mic-4-bus" "mic-5-bus" "mic-6-bus" "mic-7-bus"})

(defn init-buses-and-input-vars! []
  #_:clj-kondo/ignore
  (do
    (defonce guitar-bus (o/audio-bus 1 "guitar-bus"))
    (defonce mic-1-bus (o/audio-bus 1 "mic-1-bus"))
    (defonce mic-2-bus (o/audio-bus 1 "mic-2-bus"))
    (defonce mic-3-bus (o/audio-bus 1 "mic-3-bus"))
    (defonce mic-4-bus (o/audio-bus 1 "mic-4-bus"))
    (defonce mic-5-bus (o/audio-bus 1 "mic-5-bus"))
    (defonce mic-6-bus (o/audio-bus 1 "mic-6-bus"))
    (defonce mic-7-bus (o/audio-bus 1 "mic-7-bus"))
    (defonce texto-sonoro-bus (o/audio-bus 4 "texto-sonoro-bus")))

  (reset! inputs
          {:guitar {:in (:guitar ins)
                    :bus guitar-bus}
           :mic-1 {:in (:mic-1 ins)
                   :bus mic-1-bus}
           :mic-2 {:in (:mic-2 ins)
                   :bus mic-2-bus}
           :mic-3 {:in (:mic-3 ins)
                   :bus mic-3-bus}
           :mic-4 {:in (:mic-4 ins)
                   :bus mic-4-bus}
           :mic-5 {:in (:mic-5 ins)
                   :bus mic-5-bus}
           :mic-6 {:in (:mic-6 ins)
                   :bus mic-6-bus}
           :mic-7 {:in (:mic-7 ins)
                   :bus mic-7-bus}})

  (reset! special-inputs
          {:texto-sonoro {:in (:texto-sonoro ins)
                          :bus texto-sonoro-bus}})

  #_:clj-kondo/ignore
  (def input-number->bus*
    {0 guitar-bus
     1 mic-1-bus
     2 mic-2-bus
     3 mic-3-bus
     4 mic-4-bus
     5 mic-5-bus
     6 mic-6-bus
     7 mic-7-bus
     8 texto-sonoro-bus}))

(defn input-number->bus [n]
  (if-not (number? n)
    n
    (get input-number->bus* n n)))

(defn bus->bus-name [bus]
  (-> bus :name (str/replace #"-bus" "")))

(defn get-input-bus
  [k]
  (-> @inputs k :bus))

;;;;;;;;;;;;;;;;
;;; Texto Sonoro

(oe/defsynth texto-sonoro-rand-mixer
  [in 0
   out 0]
  (o/out out
         (o/select (lfo 1 0 3)
                   (o/sound-in (map #(+ % in) (range 4))))))

(defonce texto-sonoro-rand-mixer-bus (atom nil))

(defn init-texto-sonoro-rand-mixer-synth!
  [special-inputs*]
  (when-not @texto-sonoro-rand-mixer-bus
    (reset! texto-sonoro-rand-mixer-bus
            (o/audio-bus 1 "texto-sonoro-rand-mixer-bus")))
  (texto-sonoro-rand-mixer
   {:group (groups/early)
    :in (-> special-inputs* :texto-sonoro :in)
    :out @texto-sonoro-rand-mixer-bus}))

(comment

  (def ts (texto-sonoro-rand-mixer
           {:group (groups/early)
            :in (-> special-inputs :texto-sonoro :in)
            :out @texto-sonoro-rand-mixer-bus}))

  (o/kill ts)
  (o/demo (o/in @texto-sonoro-rand-mixer-bus 1))
  (o/kill texto-sonoro))

(comment
  (def texto-sonoro (input {:in (-> special-inputs :texto-sonoro :in)
                            :out (reaper-returns 1)}))
  (o/kill texto-sonoro)
  (-> special-inputs :texto-sonoro))

;;;;;;;;;;;;;;
;; preouts ;;;
;;;;;;;;;;;;;;

(defsynth preout
  [in 0
   out 0
   gate 1
   release 5]
  (let [env (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                       gate
                       :action o/FREE)]
    (o/out out (* env (o/in in 4)))))

(defsynth preout-8
  [in 0
   out 0
   gate 1
   release 5]
  (let [env (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                       gate
                       :action o/FREE)]
    (o/out out (* env (o/in in 8)))))

(defonce preouts (atom {}))

(defn set-preout-in!
  [input-name in-bus]
  (swap! preouts #(-> %
                      (assoc-in [input-name :bus] in-bus)
                      (update-in [input-name :synth] ctl-synth :in in-bus))))

(defn get-clean-instrument-return
  [input-name]
  (if (= :guitar input-name)
    (get-guitar-main-out)
    (get-percussion-main-out)))

(defn get-process-instrument-return
  [input-name]
  (if (= :guitar input-name)
    (get-guitar-processes-main-out)
    (get-percussion-processes-main-out)))

(defn get-mixed-instrument-return
  []
  (get-mixed-main-out))

(defn init-preouts!
  "Note that no preout is initialized for the `:texto-sonoro`"
  [inputs return-n-chans]
  (let [preout* (case return-n-chans
                  4 preout
                  8 preout-8)]
    (doseq [[_k {:keys [bus synth]}] @preouts]
      (o/free-bus bus)
      (ctl-synth2 synth :gate 0))
    (reset! preouts
            (->> inputs
                 (map (fn [[input-name _]]
                        (let [preout-bus (o/audio-bus return-n-chans (str "preout-" (name input-name)))]
                          [input-name {:bus preout-bus
                                       :synth (preout* {:group (groups/preouts)
                                                        :in preout-bus
                                                        :out (get-clean-instrument-return input-name)})}])))
                 (into {})))))

(comment
  (o/stop)
  (init-preouts! inputs)
  (-> @preouts :guitar :bus)
  (defsynth sini [out 0]
    (o/out out (* 0.2 (o/sin-osc [200 210 220 230]))))
  (def busy (o/audio-bus 4 "busy"))
  (sini {:group (groups/early) :out busy})
  (preout {:group (groups/late)
           :in busy})
  (outy {:group (groups/late)
         :in busy})

  (o/stop)

  (defsynth outy [in 0]
    (let [in* (o/in in 4)]
      (o/out (reaper-returns 1) in*)
      (o/out (reaper-returns 2) in*))))
