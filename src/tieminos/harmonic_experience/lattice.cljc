(ns tieminos.harmonic-experience.lattice
  (:require
   [clojure.string :as str]
   [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
   [quil.core :as q]))


(defn draw [text-type width height lattice-data]
  (fn []
    (let [{:keys [data edges min-x max-x min-y max-y period played-notes]
           :or {played-notes #{}}} @lattice-data
          x-length (->> [min-x max-x]
                        (map #(Math/abs %))
                        (apply +))
          y-length (->> [min-y max-y]
                        (map #(Math/abs %))
                        (apply +))
          cx (/ width 2)
          cy (/ height 2)
          zoom (* 0.7 (min (/ width x-length)
                           (/ height y-length)))]
      (q/background 0)
      (q/translate cx cy)
      (q/scale zoom)
      (comment (q/stroke 255 255)

               (q/rect min-x min-y x-length y-length))

      (q/stroke 255 255)

      (comment (q/fill 255 0)
               (q/stroke-weight 20)
               (q/point 0 0))

      (q/fill 255 255)
      #_(q/push-matrix)
      (q/translate (- (/ (+ max-x min-x) 2))
                   (- (/ (+ max-y min-y) 2)))

      (comment
        (q/stroke 255 255)
        (q/fill 255 255 0 200)
        (q/stroke-weight 2.5)
        (q/rect min-x min-y x-length y-length))

      (q/fill 255)
      (q/stroke-weight 0.2)
      (doseq [edge edges]
        (let [[coords-1 coords-2] edge]
          (q/line (:x coords-1)
                  (:y coords-1)
                  (:x coords-2)
                  (:y coords-2))))
      (q/stroke-weight 3.5)
      (doseq [{:keys [coords ratio]} data]
        (when (played-notes ratio)
          (q/stroke-weight 5.5)
          (q/stroke 150 0 200))
        (q/point (:x coords) (:y coords))
        (q/stroke-weight 3.5)
        (q/stroke 255 255))
      (q/text-font (q/create-font "Monospace" 5) 5)
      (q/stroke-weight 0)
      #_(q/fill 255 0 0)
      (doseq [{:keys [ratio coords numer-factors denom-factors]} data]
        (q/text (let [denom-factors* (str/join "*" (remove #(= period %) denom-factors))]
                  (if (= :factors @text-type)
                    (str (str/join "*" (let [ns (remove #(= period %) numer-factors)]
                                         (if (seq ns) ns [1])))
                         (when (seq denom-factors*)
                           (str "/" denom-factors*)))
                    (str ratio)))
                (+ (:x coords) 2) (- (:y coords) 0.4))))))




(defn draw-lattice
  [{:keys [ratios width height text-type]
    :or {width 800
         height 800
         text-type :ratios              ; #{:factors :ratios}
         }}]
  (let [lattice-data (atom (assoc (ratios->lattice-data base-coords
                                                        ratios)
                                  :played-notes #{}
                                  :text-type text-type))]
    (q/defsketch lattice-tool
      :title "Lattice Tool"
      :host "lattice-canvas"
      :settings #(q/smooth 80)
      :setup (fn []
               #_(q/pixel-density 2)
               (q/frame-rate 24))
      :draw (#'draw (atom text-type) width height lattice-data)
      :size [width height])
    lattice-data))
