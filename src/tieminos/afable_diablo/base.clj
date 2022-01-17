(ns tieminos.afable-diablo.base
  (:require
   [helins.interval.map :as imap]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(def elapsed-time "In seconds" (atom 0))
(defn count-time []
  (ref-rain :id :time-counter
            :durs [1]
            :on-event (on-event (swap! elapsed-time inc))))

;; WIP o usar bezier?
(def make-activity-map
  "`act-seq` is something like [[10 0.3] [10 0.7] [10 0.2] [10 0]]"
  (memoize (fn [act-seq]
             (reduce (fn [{:keys [acc-dur imap]} [dur level]]
                       (let [end (+ acc-dur dur)]
                         {:acc-dur end
                          :imap (imap/mark imap acc-dur end level)}))
                     {:acc-dur 0 :imap imap/empty}
                     act-seq))))

(defn act-level [act-map time]
  (let [iact (:imap act-map)
        cycle-dur (:acc-dur act-map)]
    (first (iact (mod time cycle-dur)))))

(comment
  (act-level (make-activity-map [[10 0.3] [10 0.7] [10 0.2] [10 0]])
             60))


(def outs {:midi-kb 5                   ; y 6
           :arps 7                      ; y 8
           :dq-highs 9                  ; y 10
           :dq-bass  11                 ; y 12
           })

(def all-outs->0 (atom false))

(defn get-out [k]
  (if @all-outs->0 0 (dec (outs k 0))))
