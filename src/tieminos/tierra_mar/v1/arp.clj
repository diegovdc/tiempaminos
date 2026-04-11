(ns tieminos.tierra-mar.v1.arp
  "A grain sample arp(eggiator)"
  (:require
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base
    :refer [interval-from-pitch-class2]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp
    :refer [arp arp-reponse-2 default-interval-seq-fn]]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.sc-utils.recording.v1 :as sc.rec.v1]
   [tieminos.tierra-mar.v1.arp :as arp]
   [tieminos.tierra-mar.v1.state :as tm.state :refer [state]]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

;; NOTE: `ge-live-sig/start-signal-analyzer' should be running

(defn stop-sample-arp! []
  (timbre/info :stopping-arp)
  (rain.v2/stop ::arp-rain)
  (swap! state assoc :arp.refrain/on? false))

(comment
  (-> @state)
  (stop-sample-arp!)
  (rain.v2/stop))

(defn start-sample-arp!
  "On every `dur` call an `arp` (arpeggio) function"
  [{:keys [state-atom
           durs
           out-fn
           group]
    :or {durs [5 3 8 2 1 5]
         out-fn (fn [_i] 0)}}]
  (timbre/info :starting-arp)
  (rain.v2/ref-rain
   :id ::arp-rain
   :durs durs
   :ratio 1/3
   :on-event (rain.v2/on-event
              (let [{:keys [arp/scale arp/pattern]} @state-atom]
                (arp {:bufs-atom sc.rec.v1/bufs
                      :dur 0.5
                      :index index
                      :in (ge.route/fl-i1 :bus)
                      :play-fn #_(partial #'arp-reponse-1 {:scale scale
                                                           :out (bh 0)})
                      (partial #'arp-reponse-2
                               (cond-> {:scale scale

                                        :amp-min 1
                                        :amp-max 1.5
                                        :interval-seq-fn (:fn pattern)
                                        :out (out-fn i) #_(rainseq (++ 25 (map #(mod % 44) (range 0 88 5))))}
                                 group (assoc :group group)))}))))
  (swap! state-atom assoc :arp.refrain/on? true))

;;;;;;;;;;;;;;;;;;
;; Patterns
;;;;;;;;;;;;;;;;;;

(defn default-interval-seq-fn
  [pitch-class scale]
  (let [direction (rand-nth [1 -1])]
    (map #(interval-from-pitch-class2 scale pitch-class %)
         (map #(* % direction)
              (range 0 (+ 9 (rand-int 7)) 2)))))

(defn make-repeat-cell
  [pattern-cell
   pitch-class
   scale
   & {:keys [min-len max-len]
      :or {min-len 3 max-len 9}}]
  (let [len (max min-len (rand-int max-len))
        pattern (->> pattern-cell
                     repeat
                     flatten
                     (take len))]
    ;; just for the UI's benefit
    (swap! state assoc :arp/pattern-str (str (into [] pattern)))

    (map #(interval-from-pitch-class2 scale pitch-class %)
         pattern)))

(defn simple-pattern
  [pattern pitch-class scale]

  ;; just for the UI's benefit
  (swap! state assoc :arp/pattern-str (str (into [] pattern)))

  (map #(interval-from-pitch-class2 scale pitch-class %)
       pattern))

(defn negate-seq [coll] (map #(* -1 %) coll))

(defn make-seq-range
  "Make a sequenential range, always starting from the zeroth degree.
  If `converge?` then the sequence will be reversed."
  [{:keys [len interval down? converge?]}]
  (let [seq* (range 0 (* interval (max 1 len)) interval)]
    (cond-> seq*
      down? negate-seq
      converge? reverse)))

(comment
  (make-seq-range {:len 0
                   :interval 3
                   :down? true
                   :converge? false}))

(let [local-state (atom {})
      get-deg (fn [index min-deg max-deg]
                (+ (mod index (- (inc max-deg) min-deg))
                   min-deg))]
  (defn stateful-rise
    [{:keys [id min-len max-len min-deg max-deg
             intervals]
      :or {intervals [1]}}
     pitch-class
     scale]

    (let [last-range-index (@local-state id 0)
          len (rrand min-len (inc max-len))
          interval (rand-nth intervals)]

      (swap! local-state assoc id (+ last-range-index len))

      (->> (range last-range-index ##Inf interval)
           (take len)
           (map #(interval-from-pitch-class2 scale pitch-class (get-deg % min-deg max-deg)))
           (reduce (fn [acc x]
                     (if (or (nil? (last acc))
                             (> x (last acc)))
                       (conj acc x)
                       (reduced acc)))
                   [])))))

(comment
  (require '[tieminos.compositions.garden-earth.base :refer [eik]])

  (stateful-rise {:id :a :min-len 4 :max-len 6 :min-deg -20 :max-deg 20
                  :intervals [1 2]}
                 "C+20"
                 (:scale eik)))

(map #(+ (mod % (- 2 -1)) -1)
     (range 10))

(def patterns
  "The pattern `fn` must take two arguments: pitch-class and scale and should return a list of ratios"
  (map-indexed
   (fn [i m] (assoc m :index i))
   [{:name ":default"
     :fn default-interval-seq-fn}
    {:name (str [0 2])
     :fn #(make-repeat-cell [0 2] %1 %2 {:max-len 15})}
    {:name (str [0 -2])
     :fn #(make-repeat-cell [0 -2] %1 %2 {:max-len 15})}
    {:name (str [0 3 1 -2])
     :fn #(make-repeat-cell [0 3 1 -2] %1 %2 {:max-len 15})}
    {:name (str [6 -6 -12 -6 0 6])
     :fn #(make-repeat-cell (shuffle [6 -6 -12 -6 0 6]) %1 %2 {:max-len 9})}
    {:name ":div-conv"
     :fn #(simple-pattern
           (make-seq-range {:len (rand-int 5)
                            :interval (inc (rand-int 4))
                            :down? (rand-nth [true false])
                            :converge? (rand-nth [true false])})
           %1 %2)}]))

(def patterns-by-name
  (reduce (fn [m {:keys [name] :as data}] (assoc m name data)) {} patterns))

(defn get-pattern
  [name]
  (if-let [p (patterns-by-name name)]
    p
    (throw (ex-info "Pattern not found" {:name name}))))

(keys patterns-by-name)

(comment
  (-> @tm.state/state)
  (->> @sc.rec.v1/bufs
       last
       last
       (into {})))

;;;;;;;;;;;;;;;;;;
;; Scales
;;;;;;;;;;;;;;;;;;

(def arp-subcps
  (mapv (fn [i m] (assoc m :index i))
        (range)
        [{:name "2)4 of 3)6 11-1.5.7.9"}
         {:name "2)4 of 3)6 9-1.5.7.11"}
         {:name "1)4 of 3)6 5.9-1.3.7.11"}]))

(defn get-scale
  [index]
  (wrap-at index arp-subcps))
