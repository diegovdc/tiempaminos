(ns tieminos.habitat.scratch.test-event-viz-refrain
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :refer [deg->freq]]
   [erv.utils.conversions :refer [cps->midi]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [org.httpkit.client :as http]))

(comment

  (http/post "http://localhost:5000/visualizer-data"
             {:body (pr-str {:events
                             [{:label ":test", :width 50, :x 0, :y 910, :color "lightgreen"}
                              {:label ":test", :width 50, :x 50, :y 947, :color "lightgreen"}
                              {:label ":test", :width 50, :x 100, :y 960, :color "lightgreen"}
                              {:label ":test", :width 50, :x 150, :y 997, :color "lightgreen"}
                              {:label ":test", :width 50, :x 200, :y 1040, :color "lightgreen"}]})}
             (fn [{:keys [status headers body error]}] ;; asynchronous response handling
               (if error
                 (println "Failed, exception is " error)
                 (println "Async HTTP GET: " status)))))

(comment
  (gp/stop)

  (defn freq->px
    ([freq] (freq->px 16 freq))
    ([scaling-factor freq]
     (int (* scaling-factor (cps->midi freq)))))

  (defn ms->px
    [ms]
    (int (/ ms 10)))

  (defn event->viz-event
    [{:keys [refrain/id dur-ms elapsed-ms freq]}]
    {:label (str id)
     :width (ms->px dur-ms)
     :x (ms->px elapsed-ms)
     :y (freq->px freq)
     :color "lightgreen"})

  (def scale (:scale (cps/make 2 [1 3 5 7])))

  (def events (atom []))

  (->> @events
       (map event->viz-event))

  (defn get-visualizer-data
    [{:keys [refrain/data synth-args meta] :as _event-data} refrain-playback-rate]
    {:refrain/id (-> data :refrain/config :id)
     :refrain-ratio  (-> data :ratio (* refrain-playback-rate))
     :dur-ms  (-> data :dur-ms (* refrain-playback-rate))
     :elapsed-ms (-> data :elapsed-ms (* refrain-playback-rate))
     :freq (:freq synth-args)
     :meta (merge meta {:args/synth synth-args})})

  (do (defn test-refrain
        [{:keys [refrain-ratio
                 on-play]
          :or {refrain-ratio 1}}]
        (ref-rain
         :id :test
         :durs [1]
         :ratio refrain-ratio
         :on-event (on-event
                    (on-play {:refrain/data data
                              :meta {:instrument :guitar}
                              :synth-args {:freq (deg->freq scale 200 (at-i (range 20)))}}))))
      (test-refrain
       (let [refrain-playback-rate 2
             end-after-ms 2000
             refrain-ratio 1/2]
         {:refrain-ratio (/ refrain-ratio refrain-playback-rate)
          :on-play (fn [event-data]
                     (let [viz-data (get-visualizer-data event-data refrain-playback-rate)]
                       (swap! events conj viz-data)
                       (when (>= (:elapsed-ms viz-data) end-after-ms)
                         (println "Ended")
                         (gp/stop (:refrain/id viz-data)))))}))))
