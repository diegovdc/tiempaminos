(ns tieminos.iem-utils
  (:require
   [overtone.osc :as osc]
   [tieminos.math.utils :refer [linlin]]))

(comment
  (def client (osc/osc-client "localhost" 7890))
  (osc/osc-debug true)
  (osc/osc-send client "/MultiEncoder/azimuth0" (float 90)))

;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;

(defn deg->iem-deg [deg]
  (if-not (> deg 180)
    deg
    (- deg 360)))

(defn +index
  ([coll] (+index 0 coll))
  ([index-offset coll]
   (map-indexed (fn [i m] (assoc m :index (+ i index-offset))) coll)))

;;;;;;;;;;;;;;;;;;
;; MultiEncoder
;;;;;;;;;;;;;;;;;;

(defn configure-multiencoder
  "Sources is a collection with `index`, `azimuth` and `elevation` values"
  [client sources]
  (let [total-sources (count sources)]
    (when (> total-sources 64)
      (throw (ex-info "Total inputs exceeds 64" {:total-sources total-sources})))
    (osc/osc-send client "/MultiEncoder/inputSetting" total-sources))

  (doseq [{:keys [elevation azimuth index]} sources]
    (osc/osc-send client (str "/MultiEncoder/azimuth" index) (float azimuth))
    (osc/osc-send client (str "/MultiEncoder/elevation" index) (float elevation))))

(do
  (defn calculate-az-ring
    "Calculate an azimuth ring for the `MultiEncoder` with sources at equal distances depending on `num-parts` given.
  The returned order is clockwise."
    ([num-parts] (calculate-az-ring 0 num-parts))
    ([offset num-parts]
     (let [degs (/ 360 num-parts)]
       (->> (range 0 360 degs)
            (mapv #(+ % offset))
            ;; format numbers as MultiEncoder expects

            (mapv deg->iem-deg)
            (mapv float)))))
  (calculate-az-ring 20 6))
(do
  (defn calculate-az-ring-elevation
    "Calculate the elevation of rings for the `MultiEncoder` at equal distances
  and symmetrically spread from the middle (0degs). Thus odd `num-rings` will
  have a ring at 0 degrees.
  The order of degrees is returned from bottom to top."
    ([num-rings
      & {:keys [total-elevation]
         :or {total-elevation 180}}]
     (let [degs (/ total-elevation num-rings)]
       (->> (range 0 total-elevation degs)
            (mapv #(- % (* (/ (dec num-rings) 2) degs)))
            (mapv float)))))
  ;; 360*0 180*1/2, 120*1, 90*3/2, 72*2, 60*5/2
  (calculate-az-ring-elevation 3 {:total-elevation 90}))

(comment
  ;; 5 rings of 10 sources each + 2 sources at top and bottom
  (let [elevs (calculate-az-ring-elevation 6)
        ring (calculate-az-ring 10)
        rings (mapv (fn [elev-deg]
                      (mapv (fn [az-deg] {:elevation elev-deg :azimuth az-deg}) ring))
                    elevs)
        sources (->> (concat [{:elevation -90 :azimuth 0}]
                             rings
                             [{:elevation 90 :azimuth 0}])
                     flatten
                     +index)]
    (configure-multiencoder client sources)))

(do
  (defn degree-range
    [min max steps]
    (let [range* (- max min)
          step-size (/ range* (dec steps))]
      (range min (+ step-size max) step-size)
      #_step-size))
  (let [size 9]
    [(degree-range -90 90 size)
     (count (degree-range -90 90 size))]))

(do
  (defn degree-seq
    [step-deg-size deg-offset steps]
    (mapv #(-> %
               (* step-deg-size)
               (+ deg-offset)
               (mod 360)
               deg->iem-deg)
          (range steps)))
  (degree-seq 60 0 7))

(defn line
  "Combine two collections (azimuths and elevations) into a lines"
  [azimuths elevations]
  (mapv (fn [az elev]
          {:azimuth az
           :elevation elev})
        azimuths
        elevations))

#_(defn matrix
    "Combine two collections (azimuths and elevations) into a matrix of size azimuths*elevations"
    [azimuths elevations]
    (for [az azimuths
          elev elevations]
      {:azimuth az
       :elevation elev}))

(comment
  ;; spiral
  (let [steps 44
        elevs (degree-range -90 90 steps)
        sources (->> (line (degree-seq 15 0 steps) elevs)
                     +index)]

    (osc/osc-send client "/MultiEncoder/masterElevation" (float 0))
    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (configure-multiencoder client sources)))
(comment
  (degree-seq 45.0 45/2 8))
(comment
  ;; updown spiral
  (let [steps 44
        elevs (degree-range -90 90 steps)
        sources (->> (concat (line (degree-seq 55 0 steps) elevs)
                             #_(line (degree-seq -15 -210 steps) (reverse elevs)))
                     +index)]

    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))
    (configure-multiencoder client sources)))

(comment
  (osc/osc-send client "/MultiEncoder/masterRoll" (float (rand 180))))

(comment
  ;; bt-arc
  (let [steps 10
        elevs (degree-range -90 90 steps)
        dome-degs [-90 -45 0 45 90] #_(degree-range -90 90)
        sources (->> (mapcat #(line (degree-seq 0 % (count elevs)) elevs)
                             dome-degs)
                     +index)]

    (osc/osc-send client "/MultiEncoder/masterRoll" (float 0))

    (configure-multiencoder client sources)
    sources
    elevs))







