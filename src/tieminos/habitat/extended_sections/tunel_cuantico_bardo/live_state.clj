(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
  (:require
   [tieminos.attractors.lorentz :as lorentz]))

(defonce touch-osc-state (atom {}))

(defonce live-state (atom {:lorentz (lorentz/init-system :x (+ 0.3 (rand 0.01))
                                                         :y (+ 0.02 (rand 0.01))
                                                         :z (+ 0.012 (rand 0.01)))}))
(defn init! [data]
  (reset! live-state
          (merge {:lorentz (lorentz/init-system :x (+ 0.3 (rand 0.01))
                                                :y (+ 0.02 (rand 0.01))
                                                :z (+ 0.012 (rand 0.01)))}
                 data)))

(defn init-watch!
  [id f]
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (f new-value))))

(comment
  (->> @live-state)
  (def lorentz (->> @live-state :lorentz))
  (lorentz 2)
  (nth [1 2 3 4] 2))

(defn synth-bank-path
  [player & keys]
  (concat [:algo-2.2.9-clouds player] keys))

(defn get-player-data
  [player & keys]
  (get-in @live-state (apply synth-bank-path player keys)))

(comment (get-player-data :milo 0 :sample-lib-size))

(defn get-selected-synth-bank
  [player]
  (get-in @live-state (synth-bank-path player :selected-bank) :default-bank))

(defn get-selected-synth-data
  [player]
  (let [bank (get-in @live-state (synth-bank-path player :selected-bank) :default-bank)]
    (get-in @live-state (synth-bank-path player bank))))

(defn selected-synth-bank-path
  [player & keys]
  (apply synth-bank-path player (get-selected-synth-bank player) keys))

(defn save-touchosc-synth-param
  ([player {:keys [path value]}]
   (save-touchosc-synth-param player path value))
  ([player osc-path value]
   (swap! live-state
          assoc-in
          (selected-synth-bank-path player :touch-osc-data osc-path)
          value)))

;;;;;;;;;;;;;;;;;;
;; Banks
;;;;;;;;;;;;;;;;;;

(defn get-active-banks
  "Get all active banks, group and independent."
  [player]
  (->> player
       get-player-data
       :refrains
       (keep (fn [[k v]] (when (:on? v) k)))
       set))

(defn get-independent-banks
  "Get banks that are independent."
  ([player] (get-independent-banks player true))
  ([player active?]
   (->> player
        get-player-data
        :refrains
        (keep (fn [[k v]] (when (and (:independent? v)
                                     (= active? (:on? v))) k)))
        set)))

(defn get-group-banks
  "Get that are not indepedent."
  ([player] (get-group-banks player true))
  ([player active?]
   (->> player
        get-player-data
        :refrains
        (keep (fn [[k v]] (when (and (not (:independent? v))
                                     (= active? (:on? v))) k)))
        set)))

(comment [(get-active-banks :milo) (get-independent-banks :milo true) (get-group-banks :milo true)])
