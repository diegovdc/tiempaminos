(ns tieminos.tierra-mar.v1.lluvia
  (:require
   [overtone.osc :as osc]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

;; * Lluvia: presentación de la voz
;; 1. voz del planeta, cadena de la vida vidagua: voz melodiza
;; 2. poco a poco la voz se estira y se mueve (az-widths)
;; 3. del centro de la voz vuelve a comenzar a salir la flauta en espirales (quizá "duplicada" por sintetizadores o pitch-shifts o algo)

;;;;;;;;;;;;;;;;;;
;; Voice paths
;;;;;;;;;;;;;;;;;;

(->> (range 1 26)
     (partition 8 8 nil))

(def ^:private levels
  (->> (range 1 26)
       (partition 8 8 nil)
       reverse))

(defn bound-path-value
  [prev-val direction min-of-level max-of-level]
  (let [x (+ direction prev-val)]

    (cond
      (< x min-of-level) max-of-level
      (> x max-of-level) min-of-level
      :else x)))

#_(reduce (fn [acc _]
            (conj acc (bound-path-value (last acc) 1 9 16)))
          [9]
          (range 1 12))

(defn- make-voice-path
  [len]
  (:path (reduce (fn [{:keys [path level direction]
                       :as acc} _]
                   (let [prev-val (last path)
                         outer-ring? (> 9 prev-val)
                         next-level? (and (not outer-ring?)
                                          (> (rand) 0.6))
                         level* (nth levels level)
                         max-of-level (last level*)
                         min-of-level (first level*)]
                     (cond
                       (zero? level)
                       (let [x (-> levels (nth 1) rand-nth)]
                         (-> acc
                             (update :path (fn [xs] (conj xs x)))
                             (update :level inc)))
                       next-level?
                       (-> acc
                           (update :path (fn [xs]  (conj xs (- prev-val 8))))
                           (update :level inc)
                           (assoc :direction (rand-nth [-1 1])))

                       :else
                       (update acc :path (fn [xs] (conj xs
                                                        (bound-path-value
                                                         prev-val
                                                         direction
                                                         min-of-level
                                                         max-of-level)))))))
                 {:path [25]
                  :direction (rand-nth [-1 1])
                  :level 0}
                 (range len))))

(defn send-lorentzian-flow-osc
  [iem-osc-client lorentz-system i]
  (let [i* (* i)
        az (lorentz/bound (lorentz-system i*) :x -60 60)
        el (lorentz/bound (lorentz-system i*) :y 0 60)
        roll (lorentz/bound (lorentz-system i*) :z -60 60)]
    (osc/osc-send iem-osc-client "/MultiEcoder/masterAzimuth" (float az))
    (osc/osc-send iem-osc-client "/MultiEncoder/masterElevation" (float el))
    (osc/osc-send iem-osc-client "/MultiEncoder/masterRoll"  (float roll))))

(defn send-lorentzian-shadow-osc
  [iem-osc-client lorentz-system i
   & {:keys [az-range el-range roll-range]
      :or {az-range [-60 60]
           el-range [0 60]
           roll-range [-60 60]}}]
  (let [i* (* i)
        [az1 az2] az-range
        [el1 el2] el-range
        [roll1 roll2] roll-range
        az (lorentz/bound (lorentz-system i*) :x az1 az2)
        el (lorentz/bound (lorentz-system i*) :y el1 el2)
        roll (lorentz/bound (lorentz-system i*) :z roll1 roll2)]
    (osc/osc-send iem-osc-client "/StereoEncoder/azimuth" (float az))
    (osc/osc-send iem-osc-client "/StereoEncoder/elevation" (float el))
    (osc/osc-send iem-osc-client "/StereoEncoder/roll"  (float roll))
    (osc/osc-send iem-osc-client "/StereoEncoder/width"  (float (* 2 (+ az el roll))))))

(defn start-lorentzian-osc!
  "OSC for the multi encoders"
  []
  (let [lor (lorentz/init-system :x 0.91 :y 0.06 :z 0.02 :dt 0.005)
        lor2 (lorentz/init-system :x 0.81 :y 0.06 :z 0.02 :dt 0.03)]
    (rain.v2/ref-rain
     :id ::lorentzian-encoders
     :durs [0.3]
     :on-event
     (rain.v2/on-event
      (send-lorentzian-flow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-voz)
       lor (+ 1000 i))
      (send-lorentzian-flow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-fl)
       lor (+ 1000 i))

      (send-lorentzian-shadow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-voz-shadow)
       lor2 (+ 1000 i))
      (send-lorentzian-shadow-osc
       (tm.configs/get-iem-osc-client :lluvia-viento-fl-shadow)
       lor2 (+ 1000 i)
       {:az-range [60 -60]
        :roll-range [60 -60]
        :el-range [-60 0]})))))
(comment
  (start-lorentzian-osc!)
  (rain.v2/stop))

#_(make-voice-path 8)
