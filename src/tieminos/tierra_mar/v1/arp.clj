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
   [tieminos.seq-utils.core :refer [++ rainseq]]
   [tieminos.tierra-mar.v1.arp :as arp]
   [tieminos.tierra-mar.v1.state :as tm.state :refer [state]]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

;; NOTE: `ge-live-sig/start-signal-analyzer' should be running

(defn stop-sample-arp! []
  (timbre/info :stopping-arp)
  (rain.v2/stop ::arp-rain)
  (swap! state assoc :arp.refrain/on? false))

(comment
  (-> @state)
  (stop-sample-arp!)
  (rain.v2/stop))

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

(defn start-sample-arp!
  "On every `dur` call an `arp` (arpeggio) function"
  [{:keys [state-atom
           interval-seq-fn
           durs
           out-fn
           group]
    :or {interval-seq-fn default-interval-seq-fn
         durs [5 3 8 2 1 5]
         out-fn (fn [_i] 0)}}]
  (timbre/info :starting-arp)
  (rain.v2/ref-rain
   :id ::arp-rain
   :durs durs
   :ratio 1/3
   :on-event (rain.v2/on-event
              (let [{:keys [arp/scale arp/pattern]} @state-atom
                    interval-seq-fn* (fn [& args]
                                       (apply interval-seq-fn args))]

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
                                        :interval-seq-fn interval-seq-fn* #_(:fn pattern)
                                        :out (out-fn i) #_(rainseq (++ 25 (map #(mod % 44) (range 0 88 5))))}
                                 group (assoc :group group)))}))))
  (swap! state-atom assoc :arp.refrain/on? true))

(comment
  ()
  (->> @sc.rec.v1/bufs
       last
       last
       (into {})))
