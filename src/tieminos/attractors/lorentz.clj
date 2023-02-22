(ns tieminos.attractors.lorentz
  (:require [clojure.spec.alpha :as s]
            [overtone.core :as o]
            [quil.core :as q]))

(def dt 0.01)
(defn dx [a x y] (* dt a (- y x)))
(defn dy [b x y z] (* dt (- (* x (- b z)) y)))
(defn dz [c x y z] (* dt (- (* x y) (* c z))))

(do
  (defn init-system
    "Creates a function with a set of initial values, that will compute their
  value of the lorentz attractor at a certain point in time.
  It will memoize previous states and will only calculate new states as necessary.

  It also returns the min and max values for each coordinate in the system at
  the latest point that has been computed.

  Usage:
  `(def lor (lorentz-system))`
  `(lor 5000)`"
    [& {:keys [x y z a b c dt]
        :or {x 0.01
             y 0.03
             z 0.02
             a 10.4
             b 29
             c 8/3
             dt 0.01}}]
    (let [state (atom {:x x :y y :z z :points []})
          get-max (fn [maxs coord v] (max (get maxs coord 0) v))
          get-min (fn [mins coord v] (min (get mins coord 0) v))]
      (fn [i]
        (if-let [points (nth (@state :points) i nil)]
          (let [{:keys [mins maxs]} @state]
            {:point points
             :mins mins
             :maxs maxs})
          (let [{:keys [points] :as s} @state
                init (count points)
                {:keys [mins maxs points]
                 :as next-state} (->> (range init (inc i))
                                      (reduce
                                       (fn [{:keys [x y z points mins maxs]} _]
                                         (let [x (+ x  (dx a x y))
                                               y (+ y (dy b x y z))
                                               z (+ z (dz c x y z))]
                                           {:x x :y y :z z
                                            :maxs {:x (get-max maxs :x x)
                                                   :y (get-max maxs :y y)
                                                   :z (get-max maxs :z z)}
                                            :mins {:x (get-min mins :x x)
                                                   :y (get-min mins :y y)
                                                   :z (get-min mins :z z)}
                                            :points (conj points [x y z])}))
                                       s))]
            (reset! state next-state)
            {:point (last points)
             :mins mins
             :maxs maxs}))))))

(defn- bounds* [lorentz-val coord]
  [(get-in lorentz-val [:mins coord] 0)
   (get-in lorentz-val [:maxs coord] 0)])

(defn bound
  [lorentz-val coord min max]
  (let [[low1 high1] (bounds* lorentz-val coord)
        v ((lorentz-val :point) (case coord :x 0 :y 1 :z 2 0))]
    (q/map-range v low1 high1 min max)))

(defn coord [lorentz-val coord]
  (-> lorentz-val :point (nth (case coord :x 0 :y 1 :z 2 0) 0)))
(defn x [lorentz-val] (-> lorentz-val (coord :x)))
(defn y [lorentz-val] (-> lorentz-val (coord :y)))
(defn z [lorentz-val] (-> lorentz-val (coord :z)))

(do (o/defsynth sound* [freq 300 amp 1 mod* 0.01]
      (o/out 0 (-> (o/sin-osc freq)
                   o/pan2
                   (* 0.1 amp (o/lf-tri:kr mod*) (o/env-gen (o/env-perc 0.01 0.5) :action o/FREE)))))
    #_(sound* :freq (bound (lor 100) :x 200 400)))

(comment
  (do
    (def lor (init-system :x 0.3 :y 0.02 :z 0.012))
    (def lor2 (init-system :x 0.31 :y 0.02 :z 0.012))
    (def lor3 (init-system :x 0.32 :y 0.02 :z 0.012))
    (def lor4 (init-system :x 0.33 :y 0.03 :z 0.012))
    (def lor5 (init-system :x 0.34 :y 0.02 :z 0.012))
    (coord (lor 510) :y)

    (lor 100)

    ;; quil visualization

    (def state (atom {:index 0})))
  (do
    (defn setup []
      (q/background 0)
      (q/stroke-weight 1)
      (q/no-fill)
      (q/color-mode :hsb))

    (def a 10.4)
    (def b 29)
    (def c 8/3)
    (def dt 0.007))
  (def color
    (memoize (fn [system-id]
               [(+ (rand 100) 100) (+ (rand 10) 144) 240])))
  (defn render-system [index system system-id]
    #_(q/background 0)
    (sound* :freq (bound (system index) :y 400 100)
            :amp (bound (system index) :x 0 2)
            :mod* (bound (system index) :z 0 10))
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/width) 2)]
      (apply q/stroke (color system-id))
      (q/begin-shape)
      (doseq [[x y] (->> index (range 0) (take-last 100) (map (comp :point system)))]
        (q/vertex (* 10 x) (* 10 y)))
      (q/end-shape)))
  (defn draw []
    (q/background 0)
    (q/stroke-weight 3)
    (q/no-fill)
    (q/stroke 255)
    (doseq [[id system] (map-indexed vector [lor lor2 lor3 lor4 lor5])]
      (render-system (@state :index) system id))
    (swap! state update :index inc))

  (q/defsketch lorentz
    :size [1000 1000]
    :setup setup
    :draw draw))
