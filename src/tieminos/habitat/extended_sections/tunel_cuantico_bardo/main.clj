(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.main
  "The code from the recorded versions of `2.3.x`, `2.2.9.x`"
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.init :as bardo.init]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc :as bardo.osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.save-synths
    :as tc.synth-persistance]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.scratch.main]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :as habitat.route]
   [tieminos.habitat.synths.granular
    :refer [clouds2-4ch]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(comment
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NOTE main initialization section
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; init OSC communication
  (bardo.osc/init!
    ;; NOTE if a client is missing there will be a "Host is Down" error.
   [["127.0.0.1" 16181]      ;; local
    #_["192.168.0.101" 16180] ;; diego
    #_["192.168.0.104" 16180] ;; milo
    ])
  (reset-default-state!)
  ;; init everything (habitat and input synths, bardo.comms) except SC, REAPER and OSC communications
  (bardo.init/all!)
  (bardo.osc/post-live-state-to-ui!)
  (bardo.osc/post-live-state-to-ui! :print-instead? true))

(defn reset-default-state!
  []
  (bardo.live-state/init-state!)
  ;; TODO: perhaps here the default-touch-osc state is duplicated and the state of the :selected-synth should be used instead?
  (doseq [[path args] bardo.live-state/default-touch-osc-state]
    (when-not (or (some #(str/ends-with? path %) ["-visible" "-label" "-group"])
                  (str/includes? path "toggle-bank"))
      (bardo.osc/osc-responder {:path path :args args})))
  (timbre/info "State has been reset"))

;; TODO: figure out if this is still useful
(defonce saved-synth-params (atom []))

(comment
  (->> @rec/bufs
       vals
       (map #(into {} %)))
  (-> @saved-synth-params)
  (tc.synth-persistance/save-params
   {:buffers-db-keyword-prefix :test/gusano-cuantico-2.2.9.2
    :params-file-name "test-gusano-cuantico-2.2.9.2.edn"
    :params-indexes [0]
    :buffers-atom rec/bufs
    :params-atom saved-synth-params}))

(comment
  (def take-1-synths
    (tc.synth-persistance/rehydrate-synth-params
     {:buffers-db-keyword-prefix :gusano-cuantico-2.2.9.2/take-1
      :params-file-name "gusano-cuantico-2.2.9.2_take-1.edn"
      :groups @groups/groups
      :default-group (groups/mid)
      :default-out (habitat.route/get-mixed-main-out)}))

  (def test-synths
    (tc.synth-persistance/rehydrate-synth-params
     {:buffers-db-keyword-prefix :test/gusano-cuantico-2.2.9.2
      :params-file-name "test-gusano-cuantico-2.2.9.2.edn"
      :groups @groups/groups
      :default-group (groups/mid)
      :default-out (habitat.route/get-mixed-main-out)}))

  ;; example for how to delete a sample
  (rec/delete-sample! (-> @(tc.synth-persistance/get-db-keyword-atom!
                            :test/gusano-cuantico-2.2.9.2)
                          vals
                          first))

  (->> take-1-synths
       (map (comp :duration :buf)))
  (->> test-synths
       first
       :buf
       (into {}))

  (gp/stop ::clouds-ref)
  (let [lor (lorentz/init-system :x 0.3 :y 0.02 :z 0.012)
        durs (take 200 (map #(-> % lor (lorentz/bound :x 2 10))
                            (range 200 40000 50)))
        reso (take 200 (map #(-> % lor (lorentz/bound :y 0 1))
                            (range 200 40000 50)))]
    (ref-rain
     :id ::clouds-ref
     :durs durs
     :on-event (on-event
                (let [start (rand)
                      end (min 1 (+ start (rand)))]
                  #_(clouds2-4ch (weighted
                                  {(-> (nth take-1-synths (weighted {0 1, 3 2}))
                                       (update :amp * 2 (rrand 0.5 1.2))
                                       #_(update :rate * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :moog-freq * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :d * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (assoc :start start
                                              :end end
                                              :pan (rrange -1 1)
                                              :moog-reso (at-i reso)))
                                   5
                                   (-> (nth take-1-synths (weighted {1 1, 2 8}))
                                       (update :amp * 2 (rrand 0.5 1.2))
                                       (update :rate * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :moog-freq * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :d * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (assoc :start start
                                              :a (rrand 2 4)
                                              :end end
                                              :pan (rrange -1 1)
                                              :moog-reso (at-i reso)))
                                   5}))
                  (clouds2-4ch (-> (rand-nth take-1-synths)
                                   (update :rate * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                   (assoc :start start

                                          :end end
                                          :pan (rrange -1 1))))))))
  (keys (:buf (nth take-1-synths 1)))

  (ndef/stop ::loop)
  (ndef/ndef
   ::loop
   (->> (range 5)
        (map (fn [i]
               (let [buf (:buf (rand-nth take-1-synths))]
                 (-> (o/play-buf 1
                                 buf
                                 :rate (* 1 (rand-nth [1 1/2 1/4 3/2]))
                                 :start-pos (rand-int (:n-samples buf))
                                 :loop true)
                     (* 4 (lfo-kr (o/rand 0.5 2) 0.2 1))
                     (o/free-verb)
                     (#(o/pan-az 4 % (lfo-kr 0.1 -1 1)))))))
        o/mix)
   {:out (habitat.route/get-mixed-main-out)}))
