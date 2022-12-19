(ns tieminos.habitat.recording
  (:require
   [clojure.string :as str]
   [tieminos.sc-utils.recording.v1 :as rec :refer [start-recording]]))

(declare make-buf-key!)

(defonce bufs (atom {}))

(defn rec-input
  [{:keys [section subsection input-name input-bus dur-s on-end msg countdown]
    :or {on-end (fn [& args])
         msg "Recording"
         countdown 5}}]
  (start-recording
   :bufs-atom bufs
   :buf-key (make-buf-key! section subsection input-name)
   :input-bus input-bus
   :seconds dur-s
   :msg msg
   :on-end on-end
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
    (str partial-name "-" sample-number)))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus]])

  (rec-input {:section "test"
              :subsection "p1"
              :input-name "guitar"
              :input-bus guitar-bus
              :dur-s 5})

  (save-samples :description "test"))
