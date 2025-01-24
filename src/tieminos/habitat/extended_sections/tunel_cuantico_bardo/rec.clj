(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec
  (:require
   [clojure.core.async :as async]
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :refer [live-state]]
   [tieminos.habitat.recording :refer [bufs rec-input]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(def section-name "gusano-cuantico-bardo")

(defn start-rec-loop!
  [{:keys [id
           input-k
           input-bus
           rec-dur-fn
           rec-pulse
           countdown
           on-rec-start]
    :or {id :rec-loop3
         rec-dur-fn (fn [_] 0.5)
         rec-pulse [0.5]
         countdown 0
         on-rec-start (fn [_])}}]
  (ref-rain
   :id id
   :durs rec-pulse
   :on-event (on-event
              (let [dur-s (-> (rec-dur-fn {:index index})
                              (- 0.05)
                              (max 0.01))
                    active-bank (-> @live-state
                                    :rec
                                    input-k
                                    (:active-bank 0))]
                (rec-input {:section section-name
                            :subsection active-bank
                            :input-name (:name input-bus)
                            :input-bus input-bus
                            :dur-s dur-s
                            :on-end (fn [_])
                            :print-info? false
                            :countdown countdown
                            :on-rec-start on-rec-start})))))

(defn get-buf
  ;; FIXME get lib-size from each bank
  [player-k lib-size active-banks-set]
  (->> @bufs
       (filter (fn [[_k {:keys [rec/meta]}]]
                 (and
                  (= (:section meta) section-name)
                  (active-banks-set (:subsection meta))
                  (str/includes? (:input-name meta)
                                 (if (= player-k :milo)
                                   "mic-"
                                   "guitar-")))))
       (sort-by (comp :rec/time second))
       reverse
       (take lib-size)
       (#(when (seq %) (rand-nth %)))))

(comment
  (get-buf :milo 10000 #{0 1 2 3 4 5 6 7}))

(defn delete-bank-bufs
  [input-k bank]
  (let [bank-bufs (->> @bufs
                       (filter
                        (fn [[_k {:keys [rec/meta]}]]
                          (and (= (:section meta) section-name)
                               (= (:subsection meta) bank)
                               (= (:input-name meta) (format "%s-bus" (name input-k)))))))]

    (timbre/info (format "Freeing %s buffers from bank %s of %s"
                         (count bank-bufs)
                         bank
                         input-k))
    (doseq [[_k buf] bank-bufs] (o/buffer-free buf))
    (swap! bufs (fn [bufs-map] (apply dissoc bufs-map (keys bank-bufs))))))

(comment
  (->> @bufs
       vals
       (sort-by :id))

  (o/go
    (doseq [buf bufs] (o/buffeassr-free buf))))
