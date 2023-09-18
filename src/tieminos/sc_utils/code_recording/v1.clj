(ns tieminos.sc-utils.code-recording.v1
  (:require
   [clojure.edn :as edn]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  ;; Usage
  (rec-code
   ::my-rec-id
   (gp/ref-rain
    :id :a
    :durs [1]
    :on-event (gp/on-event
               (println i))))

  (save-to-file! ::a "test-filename")

  (replay ::my-replay-id (read-file "test-filename")
          :print-replayed-code? true))

(defonce recordings (atom {}))

(defn record-event
  [list-id code-event]
  (swap! recordings update list-id conj {:timestamp (java.util.Date.)
                                         :event code-event}))

(defmacro rec-code
  [id code]
  (record-event id code)
  code)

(def recorded-code-path (str (System/getProperty "user.dir")
                             "/recordings/code/"))

(defn add-deltas
  [events-vector]
  (let [sorted-events (sort-by :timestamp
                               #(compare %1 %2)
                               events-vector)]
    (reduce
     (fn [acc {:keys [timestamp] :as ev}]
       (let [prev-event (last acc)]
         (println prev-event ev)
         (conj acc
               (assoc ev :eval-delta-ms
                      (if-not prev-event
                        0
                        (- (inst-ms timestamp)
                           (inst-ms (:timestamp prev-event))))))))
     []
     sorted-events)))

(defn make-path [filename] (str recorded-code-path filename ".edn"))

(defn save-to-file!
  [id filename]
  (let [path (make-path filename)
        content (add-deltas (get @recordings id))]
    (if (seq content)
      (do (spit path content)
          (println "File saved: " path))
      (println "Nothing to save"))))

(defn read-file [filename]
  (edn/read-string (slurp (make-path filename))))

(defn replay
  [id events & {:keys [print-replayed-code?]}]
  (let [durs (concat (->> events
                          (map (comp #(/ % 1000) :eval-delta-ms))
                          (drop 1))
                     [1])]
    (gp/ref-rain
     :id id
     :durs durs
     :teop 60
     :loop? false
     :on-event (gp/on-event
                (let [event (nth events i)
                      code (:event event)]
                  (when print-replayed-code?
                    (println "Replaying:" (:timestamp event))
                    (println code))
                  (eval code))))))
