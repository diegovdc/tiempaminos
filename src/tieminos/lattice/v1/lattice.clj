(ns tieminos.lattice.v1.lattice
  (:require
   [clojure.string :as str]
   [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
   [quil.core :as q]))

(defn draw* [text-type width height lattice-data]
  (let [lattice-data* @lattice-data
        {:keys [data edges min-x max-x min-y max-y period played-notes]} lattice-data*
        x-length (->> [min-x max-x]
                      (map #(Math/abs %))
                      (apply + 1)) ;; prevent div by zero below
        y-length (->> [min-y max-y]
                      (map #(Math/abs %))
                      (apply + 1)) ;; prevent div by zero below
        cx (/ width 2)
        cy (/ height 2)
        zoom (* 0.7 (min (/ width x-length)
                         (/ height y-length)))]
    (q/background 0)
    (q/translate cx cy)
    (q/scale zoom)

    (q/stroke 255 255)

    (q/fill 255 255)
    #_(q/push-matrix)
    (q/translate (- (/ (+ max-x min-x) 2))
                 (- (/ (+ max-y min-y) 2)))

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
      (when-let [notes (seq (played-notes ratio))]
        ;; TODO using reverse may not be too performant
        (doseq [{:keys [index color stroke-weight]
                 :or {color [170 0 80]
                      stroke-weight 3.5}} (reverse notes)]
          (q/stroke-weight (+ stroke-weight (* index 2)))
          (apply q/stroke color)
          (q/point (:x coords) (:y coords))))
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
              (+ (:x coords) 2) (- (:y coords) 0.4)))))

(defn draw [text-type width height lattice-data]
  (fn [] (draw* text-type width height lattice-data)))

(defn draw-lattice
  [{:keys [ratios width height text-type on-close]
    :or {width 800
         height 800
         on-close (fn [])
         text-type :ratios              ; #{:factors :ratios}
         }}]
  (let [lattice-data (atom (assoc (ratios->lattice-data base-coords
                                                        ratios)
                                  :played-notes {}
                                  :text-type text-type))]
    (q/defsketch lattice-tool
      :title "Lattice Tool"
      :host "lattice-canvas"
      :settings #(q/smooth 80)
      :setup (fn []
               #_(q/pixel-density 2)
               (q/frame-rate 24))
      :draw (#'draw (atom text-type) width height lattice-data)
      :on-close (fn [] (on-close))
      :size [width height])
    lattice-data))

(comment
  (def lattice-atom (draw-lattice
                     {:ratios (into #{} [1 3/2 5/4])}))

  (add-played-ratio lattice-atom {:ratio 1 :group-id 0 :color [200 0 0]})
  (remove-played-ratio lattice-atom {:ratio 1 :group-id 1})
  (add-played-ratio lattice-atom {:ratio 1 :group-id 1 :color [0 200 0]}))

(defn add-played-ratio*
  [lattice-data {:keys [_group-id ratio _color]
                 :as ratio-data}]
  (update-in
   lattice-data
   [:played-notes ratio]
   (fn [data]
     (->> ((fnil conj []) data ratio-data)
          (map-indexed (fn [i ratio-data] (assoc ratio-data :index i)))
          (into [])))))

(defn add-played-ratio
  [lattice-atom {:keys [_group-id _ratio _color]
                 :as ratio-data}]
  (swap! lattice-atom add-played-ratio* ratio-data))

(defn remove-first [pred coll]
  (let [[pre post] (split-with (comp not pred) coll)]
    (concat pre (rest post))))

(remove-first #(= % 2) [])

(defn remove-played-ratio*
  [lattice-data {:keys [group-id ratio]
                 :as _ratio-data}]
  (update-in
   lattice-data
   [:played-notes ratio]
   (fn [data]  (->> data
                    (remove-first #(= (:group-id %) group-id))
                    (map-indexed (fn [i ratio-data] (assoc ratio-data :index i)))))))

(defn remove-played-ratio
  [lattice-atom {:keys [_group-id _ratio]
                 :as ratio-data}]
  (swap! lattice-atom remove-played-ratio* ratio-data))

(defn remove-all-played-ratios
  [lattice-atom]
  (swap! lattice-atom assoc :played-notes {}))

(defn update-ratios!
  [lattice-atom new-ratios]
  (swap! lattice-atom
         merge
         (ratios->lattice-data base-coords
                               new-ratios)))
