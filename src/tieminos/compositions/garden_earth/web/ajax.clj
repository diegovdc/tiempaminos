(ns tieminos.compositions.garden-earth.web.ajax
  (:require
   [org.httpkit.client :as http]
   [tieminos.compositions.garden-earth.base :refer [pitch-class->pr-fingering]]))

(defn post [endpoint body & {:keys [debug?]}]
  (http/post (str "http://localhost:5000" endpoint)
             {:body (pr-str body)}
             (fn [{:keys [status headers body error]}] ;; asynchronous response handling
               (if error
                 (println "Failed, exception is " error endpoint)
                 (when debug? (println "Async HTTP POST: " status))))))

(defn post-fingering
  [pitch-class]
  (post "/garden-earth/tuning/fingering"
        (pitch-class->pr-fingering pitch-class)))

(defn post-note-tuning
  [data]
  (post "/garden-earth/tuning/note-tuning" data))

(defn post-live-state
  [data]
  (post "/garden-earth/live-state" data))


(comment
  (post-fingering "D+24")
  (post-fingering "C#+71")
  (post-note-tuning {:pitch-class "C#+71"
                     :diff-cents -1.5087897})

  (post-live-state {:s&h.refrain/on? true
                    :harmonizer/on? true
                    :harmonizer/harmony-str (str [3/2 7/6])})
  )
