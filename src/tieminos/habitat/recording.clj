(ns tieminos.habitat.recording
  (:require
   [clojure.math :refer [round]]
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.habitat.routing :refer [bus->bus-name input-number->bus*]]
   [tieminos.sc-utils.recording.v1 :as rec :refer [start-recording]]
   [tieminos.utils :refer [avg hz->ms]]))

(declare make-buf-key! add-analysis)

(defonce bufs (atom {}))

(defn rec-input
  [{:keys [section subsection input-name input-bus dur-s on-end msg countdown]
    :or {on-end (fn [& args])
         msg "Recording"
         countdown 0}}]
  (start-recording
   :bufs-atom bufs
   :buf-key (make-buf-key! section subsection input-name)
   :input-bus input-bus
   :seconds dur-s
   :msg msg
   :on-end (fn [buf-key]
             (add-analysis dur-s buf-key input-bus)
             (on-end buf-key))
   :countdown countdown))

(defn save-samples [& {:keys [description full-keyword]}]
  (let [prefix  (or full-keyword
                    (keyword "habitat"
                             (str "*"
                                  (-> (.format (java.time.ZonedDateTime/now)
                                               java.time.format.DateTimeFormatter/ISO_INSTANT))
                                  (when description (str "*" (str/replace description #" " "-"))))))]
    (rec/save-samples prefix
                      :path (str (System/getProperty "user.dir")
                                 "/samples/habitat_samples/")
                      :buffers-atom bufs)))

(defonce buf-keys-count (atom {}))

(defn- make-buf-key! [section subsection input-name]
  (let [partial-name (format "%s-%s-%s" section subsection input-name)
        sample-number (get (swap! buf-keys-count update partial-name #(if % (inc %) 1))
                           partial-name)]
    (keyword (str partial-name "-" sample-number))))

(defn rec-controller
  [{:keys [in section subsection dur-s]
    :as osc-data}]
  (let [input-bus (input-number->bus* in)]
    (if (and input-bus section subsection dur-s)
      (rec-input {:section section
                  :subsection subsection
                  :input-name (bus->bus-name input-bus)
                  :input-bus input-bus
                  :dur-s dur-s})
      (throw (ex-info "Data missing from osc command for recording"
                      {:osc-data osc-data})))))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus]])
  (-> guitar-bus :name (str/replace #"-bus" ""))
  (rec-input {:section "test"
              :subsection "p1"
              :input-name "guitar"
              :input-bus guitar-bus
              :dur-s 5})

  (save-samples :description "test"))

;;;;;;;;;;;;;;;;;;;
;; Signal Analysis
;;;;;;;;;;;;;;;;;;;

(defonce analysis-history (atom {}))

(defn run-get-signal-analysis
  [& {:keys [freq input-bus analysis-path]
      :or {freq 5}}]
  ((o/synth (let [signal (o/in:kr input-bus)]
              (o/send-reply:kr (o/impulse:kr freq) analysis-path
                               [(o/amplitude:kr signal) (o/pitch:kr signal)]
                               signal)))))
(round (/ (* 60 1000) 5))
(defn run-receive-analysis
  "Gets analysis from `in` 0 in almost real time
  and `conj`es the data into`analysis-history.
  NOTE: `run-get-signal-pitches` must be running`"
  [& {:keys [freq input-bus-name-keyword analysis-path]}]
  (let [handler-name (keyword (str (str/replace analysis-path #"/" "")
                                   "-handler"))
        one-minute-of-data (round (/ (* 60 1000) freq))
        sample-rate (hz->ms freq)]
    (o/on-event analysis-path
                (fn [data]
                  (let [[_node-id input-bus amp freq freq?*] (-> data :args)
                        freq? (= freq?* 1.0)]
                    (swap! analysis-history
                           update
                           input-bus-name-keyword
                           (comp (partial take one-minute-of-data) conj)
                           {:amp amp
                            :timestamp (o/now)
                            :freq freq
                            :freq? freq?})))
                handler-name)
    handler-name))

(def analyzer-freq 5)

(defn start-signal-analyzer
  [& {:keys [input-bus]}]
  (let [analysis-path (format "/receive-%s-analysis" (:name input-bus))]
    {:receiver (run-receive-analysis
                :freq analyzer-freq
                :input-bus-name-keyword (keyword (:name input-bus))
                :analysis-path analysis-path)
     :analyzer (run-get-signal-analysis
                :freq analyzer-freq
                :input-bus input-bus
                :analysis-path analysis-path)}))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus
                                              mic-4-bus
                                              preouts]])
  (o/stop)
  (-> @analysis-history :mic-2-bus)
  (reset! analysis-history {})
  (-> mic-2-bus :name)
  (start-signal-analyzer :input-bus mic-2-bus))

(defn add-analysis [dur-s buf-key input-bus]
  #_(swap! bufs buf-key assoc :analysis)
  (let [now (o/now)
        sample-start (- now (* 1000 dur-s)
                        ;; ensure samples window corresponds to dur-s
                        (hz->ms analyzer-freq))
        analysis (->> (get @analysis-history (keyword (:name input-bus)))
                      (drop-while #(> (:timestamp %) now))
                      (take-while #(>= (:timestamp %) sample-start))
                      (reduce (fn [acc {:keys [amp]}]
                                (-> acc
                                    (update :min-amp min amp)
                                    (update :max-amp min amp)
                                    (update :amps conj amp)))

                              {:min-amp 0
                               :max-amp 0
                               :amps ()}))
        buf (@bufs buf-key)]
    (swap! bufs update buf-key
           assoc :analysis (-> analysis
                               (assoc :avg-amp (avg (:amps analysis)))
                               (dissoc :amps)))))
(comment
  ;; TODO left here
  (add-analysis 2 :amanecer-subsection-mic-2-2 mic-2-bus))

(comment
  (rec-input {:section "amanecer"
              :subsection "subsection"
              :input-name "mic-2"
              :input-bus mic-2-bus
              :dur-s 2}))





