(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :refer [dispatch]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :refer [live-state]]
   [tieminos.habitat.recording :refer [bufs rec-input]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(def section-name "gusano-cuantico-bardo")

(defn start-rec-loop!
  [{:keys [id
           input-k
           input-bus
           rec-dur-fn
           rec-pulse
           countdown
           on-rec-start
           on-rec-end]
    :or {id :rec-loop3
         rec-dur-fn (fn [_] 0.5)
         rec-pulse [0.5]
         countdown 0
         on-rec-start (fn [_])
         on-rec-end (fn [_])}}]
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
                            :on-end on-rec-end
                            :print-info? false
                            :countdown countdown
                            :on-rec-start on-rec-start})))))

(defn stop-rec-loop!
  [id]
  (gp/stop id))

(defn get-buf
  ;; FIXME: get lib-size from each bank
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

;;;;;;;;;;;;;;;;;;;;
;; Buffer Deletion
;;;;;;;;;;;;;;;;;;;;

(defonce currently-playing-bufs (atom {}))

(defonce ^:private buffer-freeing-chan (a/chan (a/sliding-buffer 1024)))

(defonce ^:private buffer-freeing-loop-running? (atom false))

(defonce ^:private currently-playing-bufs-cleaning-loop-running? (atom false))

(defn- async-free-buffer
  ([buf]
   (a/put! buffer-freeing-chan {:buf buf})))

(defn- all-synths-inactive? [synths]
  (every? (comp not o/node-active?) synths))

(defn- inactive-buffer? [buf]
  (let [synths (get @currently-playing-bufs buf)]
    (if-not synths
      true
      (all-synths-inactive? (get @currently-playing-bufs buf)))))

(defn- start-buffer-freeing-loop!
  "Starts a loop that frees all buffers that are not being used."
  []
  (if @buffer-freeing-loop-running?
    (timbre/warn "buffer-freeing-loop already running")
    (do
      (timbre/info "Initializing buffer-freeing-loop")
      (reset! buffer-freeing-loop-running? true)
      (a/go-loop []
        (let [{:keys [buf timeout]} (a/<! buffer-freeing-chan)]
          (when timeout
            (timbre/info "Waiting for synth to stop for buf:" buf)
            (a/<! (a/timeout timeout)))
          (if (inactive-buffer? buf)
            (o/buffer-free buf)
            (a/put! buffer-freeing-chan {:buf buf :timeout 2000}))
          (if @buffer-freeing-loop-running?
            (recur)
            (timbre/info "Stopping buffer freeing loop")))))))

(defn- clean-currently-playing-bufs
  [currently-playing-bufs-data]
  (->> currently-playing-bufs-data
       (map (fn [[buf synths]]
              (let [active-synths (filter o/node-active? synths)]
                (if (seq active-synths)
                  {buf active-synths}
                  {}))))
       (apply merge)))

(defn start-currently-playing-bufs-cleaning-loop!
  "To avoid memory leaks, creates a loop that minute removes buffers, which are
  no longer being used by a synth, from the `currently-playing-bufs` atom."
  []
  (if @currently-playing-bufs-cleaning-loop-running?
    (timbre/warn "currently-playing-bufs-cleaning-loop already running")
    (do
      (timbre/info "Initializing currently-playing-bufs-cleaning-loop")
      (reset! currently-playing-bufs-cleaning-loop-running? true)
      (a/go-loop []
        #_(timbre/info "Cleaning currently-playing-bufs")
        (swap! currently-playing-bufs clean-currently-playing-bufs)
        (a/<! (a/timeout (* 60 1000)))
        (if @currently-playing-bufs-cleaning-loop-running?
          (recur)
          (timbre/info "Stopping currently-playing-bufs-cleaning-loop"))))))

(defn delete-bank-bufs
  [{:keys [input-ks banks]}]
  (let [input-ks* (->> input-ks
                       (mapv #(format "%s-bus" (name %)))
                       set)
        banks* (set banks)
        bank-bufs (->> @bufs
                       (filter
                        (fn [[_k {:keys [rec/meta]}]]
                          (and (= (:section meta) section-name)
                               (banks* (:subsection meta))
                               (input-ks* (:input-name meta)))))
                       (take 1024))]

    (timbre/info (format "Freeing %s buffers from banks %s of %s"
                         (count bank-bufs)
                         banks
                         input-ks))
    (when-not @buffer-freeing-loop-running? (start-buffer-freeing-loop!))
    (when-not @currently-playing-bufs-cleaning-loop-running? (start-currently-playing-bufs-cleaning-loop!))
    (doseq [[_k buf] bank-bufs] (async-free-buffer buf))
    (swap! bufs (fn [bufs-map] (apply dissoc bufs-map (keys bank-bufs))))))

(comment
  (doseq [_ (range 10)]
    (swap! bufs
           assoc (random-uuid)
           (assoc (o/buffer 1024)
                  :rec/meta {:input-name "guitar-bus"
                             :section section-name
                             :subsection 1}))))

(defn init-rec-buffers-manager!
  []
  (start-buffer-freeing-loop!)
  (start-currently-playing-bufs-cleaning-loop!))

(defn stop-rec-buffers-manager!
  []
  (reset! buffer-freeing-loop-running? false)
  (reset! currently-playing-bufs-cleaning-loop-running? false))

(defn count-bufs-per-bank
  [bufs-data]
  (->> bufs-data
       (reduce
        (fn [acc [_ {:keys [rec/meta] :as _buf}]]
          (update-in acc
                     [(keyword (:input-name meta))
                      (:subsection meta)]
                     (fnil inc 0)))
        {})))

(defn on-bufs-change
  [_ _ _ new-bufs]
  (dispatch {:type :bardo.event/bufs-counted
             :data (count-bufs-per-bank new-bufs)}))

(defn init-bufs-watch!
  []
  (add-watch bufs ::bufs #'on-bufs-change))
(comment
  (init-bufs-watch!))
