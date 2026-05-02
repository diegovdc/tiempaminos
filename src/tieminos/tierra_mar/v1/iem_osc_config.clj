(ns tieminos.tierra-mar.v1.iem-osc-config
  (:require
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.iem-utils :as iem.utils]
   [tieminos.tierra-mar.v1.configs :as tm.configs]))

(comment
  (def client (osc/osc-client "localhost" 7890))
  (osc/osc-debug true)
  (osc/osc-send client "/MultiEncoder/azimuth0" (float 90)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; MultiEncoder Shapes
;;;;;;;;;;;;;;;;;;;;;;;;

(defn deduplicate-sources
  [sources]
  (let [srcs (:sources (reduce
                        (fn [{:keys [sources sources-set elevations] :as acc} src]
                          (let [elev (:elevation src)]
                            (if (or (sources-set src)
                                    (elevations elev))
                              acc
                              {:sources (conj sources src)
                               :sources-set (conj sources-set src)
                               :elevations (if (#{-90 90} (abs elev))
                                             (conj elevations elev)
                                             elevations)})))
                        {:sources []
                         :sources-set #{}
                         :elevations #{}}
                        sources))]
    (when (not= (count sources) (count srcs))
      (timbre/info "removed duplicate sources:" (- (count sources) (count srcs))))
    srcs))

(comment
  ;; arpa spiral
  (let [client (tm.configs/get-iem-osc-client :olivo-spiral-arp)
        steps 28
        elevs (iem.utils/degree-range -90 90 steps)
        sources (->> (iem.utils/line (iem.utils/degree-seq 15 0 steps) elevs)
                     iem.utils/+index)]

    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (iem.utils/configure-multiencoder client sources))

  ;; arpa spiral (version para "La Vida del Olivo"
  (let [client (tm.configs/get-iem-osc-client :olivo-spiral-arp)
        steps (/ 28 2)
        elevs (iem.utils/degree-range -90 90 steps)
        deg-step 38
        sources (->> (concat
                      (iem.utils/line
                       (iem.utils/degree-seq deg-step 0 steps)
                       elevs)
                      (iem.utils/line
                       (iem.utils/degree-seq deg-step -180 steps)
                       elevs))
                     deduplicate-sources
                     iem.utils/+index)]

    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterAzimuth" (float 0))
    (iem.utils/configure-multiencoder client sources)
    (Thread/sleep 500)
    sources
    #_(osc/osc-send client "/MultiEncoder/masterRoll" (float 90)))

  ;; Copa del Olivo
  (let [client (tm.configs/get-iem-osc-client :olivo-copa)
        elevs (iem.utils/degree-range -10 70 4)
        rings (mapv (fn [i elev-deg]
                      (let [ring (iem.utils/calculate-az-ring (* i (/ 180 7)) 7)]
                        (mapv (fn [az-deg] {:elevation elev-deg :azimuth az-deg}) ring)))
                    (range)
                    elevs)
        sources (->> rings
                     flatten
                     iem.utils/+index)]
    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterAzimuth" (float 0))
    (iem.utils/configure-multiencoder client sources)
    sources)

  (iem.utils/degree-seq 20 20 5)

  ;; voz lluvia-viento
  (let [client (tm.configs/get-iem-osc-client :lluvia-viento-voz)
        elevs (drop-last 1 (iem.utils/degree-range -30 90 4))
        ring (iem.utils/calculate-az-ring 8)
        rings (mapv (fn [elev-deg]
                      (mapv (fn [az-deg] {:elevation elev-deg :azimuth az-deg}) ring))
                    elevs)
        sources (->> (conj rings {:elevation 90 :azimuth 0})
                     flatten
                     iem.utils/+index)]
    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterAzimuth" (float 0))
    (iem.utils/configure-multiencoder client sources))

  ;; fl lluvia-viento
  (let [client (tm.configs/get-iem-osc-client :lluvia-viento-fl)
        steps (/ 28 4)
        elevs (reverse (iem.utils/degree-range -90 90 steps))
        deg-step 38
        sources (->> (concat
                      (iem.utils/line
                       (iem.utils/degree-seq deg-step 0 steps)
                       elevs)
                      (iem.utils/line
                       (iem.utils/degree-seq deg-step 90 steps)
                       elevs)
                      (iem.utils/line
                       (iem.utils/degree-seq deg-step -180 steps)
                       elevs)
                      (iem.utils/line
                       (iem.utils/degree-seq deg-step -90 steps)
                       elevs))
                     deduplicate-sources
                     iem.utils/+index)]

    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterAzimuth" (float 0))
    (iem.utils/configure-multiencoder client sources)
    (Thread/sleep 500)
    sources
    #_(osc/osc-send client "/MultiEncoder/masterRoll" (float 90)))

  ;; campo-magnetismo
  ;; TODO: deduplicate poles
  (let [client (tm.configs/get-iem-osc-client :campo-magnetismo)
        steps 9
        elevs (iem.utils/degree-range -90 90 steps)
        dome-degs (iem.utils/degree-range -110 110 7)
        sources (->> (mapcat #(iem.utils/line (iem.utils/degree-seq 0 % (count elevs)) elevs)
                             dome-degs)
                     deduplicate-sources
                     iem.utils/+index)]

    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterAzimuth" (float 0))
    (iem.utils/configure-multiencoder client sources)

    (Thread/sleep 500)
    (osc/osc-send client "/MultiEncoder/masterRoll" (float 90))

    (osc/osc-send client "/MultiEncoder/masterElevation" (float 90))))




