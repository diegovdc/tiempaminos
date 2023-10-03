(ns tieminos.utils
  (:require
   [clojure.core.async :as a]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]))

(defn wrap-at [i coll]
  (let [i* (mod i (count coll))]
    (nth coll i*)))

(defn rrange
  "Random range"
  [min max]
  (+ min (rand (- max min))))

(defn period [seconds durs]
  (let [ratio (/ seconds (apply + durs))]
    (mapv #(* ratio %) durs)))

(defn periods [seconds & durs]
  (mapcat (partial period seconds) durs))

(defn gen-chord [scale fundamental [gens degs]]
  "Get a set of frequencies by passing in a cps scale, a fundamental and a vector with generators and degrees"
  (let [scale** (->> scale (#(cps/filter-scale % gens)))]
    (map (partial scale/deg->freq scale** fundamental) degs)))

(comment
  (gen-chord (:scale (cps/make 2 [1 3 5 7]))
             200
             [[1 3] [2 3 4]])
  )

;; Range mapping https://github.com/supercollider/supercollider/blob/18c4aad363c49f29e866f884f5ac5bd35969d828/lang/LangSource/MiscInlineMath.h


(defn linexp
  "Maps a value from a linear range to an exponential range"
  [lin-min lin-max exp-min exp-max value]
  (cond
    (or (<= exp-min 0) (<= exp-max 0)) (throw (ex-info "Values for the exponential range must be larger than 0"
                                                       {:lin-min lin-min
                                                        :lin-max lin-max
                                                        :exp-min exp-min
                                                        :exp-max exp-max}))
    (<= value lin-min) exp-min
    (>= value lin-max) exp-max
    :else (* exp-min (Math/pow (/ exp-max exp-min)
                               (/ (- value lin-min)
                                  (- lin-max lin-min))))))

;;; xrepeat
(def xrepeat
  "Takes a `pattern` (a seq of numbers) and a seq of any value (`xs`)
  and will produce a seq with the values of `xs` repeated by the `pattern`

  e.g.
  (mosify [3 3 2] [:a :b]) => '(:a :a :a, :b :b :b, :a :a,
                                :b :b :b, :a :a :a, :b :b)

  If `infinite?` is true will lazyly produce an infinite sequence.
  Useful when `xs` is longer than `pattern`. Remember to either get an index or use take.

  `all-xs?` ensures that the pattern cycle is complete and repeats itself correctly,
  set to false if you only care of one cycle.

  This function is memoized so that it can be performantly used inside a loop."
  (memoize
   (fn [pattern xs & {:keys [infinite? all-xs?]
                      :or {all-xs? true}}]
     (let [pattern* (cond
                      infinite? (flatten (repeat pattern))

                      all-xs? (flatten (repeat (count xs) pattern))
                      :else pattern)]
       (flatten (map-indexed (fn [i n] (repeat n (wrap-at i xs)))
                             pattern*))))))

(defn seconds->dur [secs bpm] (* secs (/ bpm 60)))

(defn dur->bpm [dur-ms] (/ 60000 dur-ms))

(defn ctl-synth [synth & params]
  (if synth
    (try (apply o/ctl synth (flatten params))
         (catch Exception e (timbre/error e)))
    (timbre/error "No synth to control")))

(defn ctl-synth2
  "Does not log any errors"
  [synth & params]
  (when synth
    (try (apply o/ctl synth (flatten params))
         (catch Exception e nil))))

(defn cps->tidal-scale
  "Probably only works for scales of period 2"
  [cps]
  (->> cps :scale
       (map #(-> %
                 :bounded-ratio
                 conv/ratio->cents
                 (/ 100)))))

(defn hz->ms
  [hz]
  (/ 1000 hz))

(defn map-subscale-degs
  "Use `scale-size` and `subscale-degs` to calculate a degree
  from the subscale in the parent scale to allow for playing with the degrees in different periods."
  [scale-size subscale-degs deg]
  (let [deg-class (wrap-at deg subscale-degs)]
    (+ (*  scale-size (+ (if (and (not (zero? deg-class))
                                  (> 0 deg))
                           -1 0)
                         (quot deg (count subscale-degs))))
       (* (wrap-at deg subscale-degs)))))

(comment
  (map-subscale-degs
   20
   [0 5 8 9]
   -8))

(defn normalize-amp
  "When passed the peak-amp of a buffer, returns the value of the amp to normalize the buffer"
  ([peak-amp] (normalize-amp peak-amp 1))
  ([peak-amp target-peak]
     ;; prevent infinite amp
   (if-not (zero? peak-amp)
     (/ target-peak peak-amp)
     target-peak)))

(defn indexes-of [el coll] (keep-indexed #(when (= el %2) %1) coll))

(defn seconds->dur [secs bpm] (* secs (/ bpm 60)))

(defn dur->bpm [dur-ms] (/ 60000 dur-ms))

(defn iter-async-call
  "Iterative async call. Will pass the index of the call to the fn.
  Useful for sequencing `o/ctl` calls."
  ([f] (iter-async-call 200 f))
  ([interval-ms f]
   (timbre/warn "iter-async-call has been deprecated use iter-async-call2")
   (let [stop-chan-fn (a/chan)]
     (a/go-loop [i 0]
       (a/alt!
         (a/timeout interval-ms) (do (a/thread (f i))
                                     (recur (inc i)))
         stop-chan-fn (timbre/debug "Stopping iter-async-call...")))
     ;; memoize to prevent a subsequent call from stopping the thread
     (memoize #(a/>!! stop-chan-fn true)))))

(defn iter-async-call2
  "Iterative async call. Will pass `index` and `stop-chan-fn` to `f`.
  Also automatically stops if there is an uncaught error when calling `f`.

  Useful for sequencing `o/ctl` calls."
  ([f] (iter-async-call 200 f))
  ([interval-ms f]
   (let [stop-chan (a/chan)
         stop-chan-fn (memoize #(a/>!! stop-chan true))]
     (a/go-loop [i 0]
       (a/alt!
         (a/timeout interval-ms) (do (a/thread
                                       (try
                                         (f {:index i :stop-chan-fn stop-chan-fn})
                                         (catch Exception e
                                           (do (timbre/error "Stopping iter-async-call2 because of error in function"
                                                             e)
                                               (stop-chan-fn)))))
                                     (recur (inc i)))
         stop-chan (timbre/debug "Stopping iter-async-call2...")))
     ;; memoize to prevent a subsequent call from stopping the thread
     stop-chan-fn)))

(comment
  (def stop-me (iter-async-call 1000 #(do (println "hola" %)
                                          (flush))))
  (def stop-me2 (iter-async-call 500 #(do (println "adios" %)
                                          (flush))))
  (stop-me)
  (stop-me2))

(defonce async-channels (atom {}))

(defn init-async-seq-call-loop!
  []
  (if-not (and (:seq-call-chan @async-channels)
               (:seq-call-loop @async-channels))
    (let [seq-call-chan (a/chan)
          seq-call-loop (a/go-loop
                         []
                          (let [{:keys [timeout f shutdown-loop?]} (a/<! seq-call-chan)]
                            (when-not shutdown-loop?
                              (f)
                              (a/<! (a/timeout timeout))
                              (recur))))]
      (swap! async-channels assoc
             :seq-call-chan seq-call-chan
             :seq-call-loop seq-call-loop)
      (timbre/info "async-seq-call channels initialized"))
    (timbre/warn "async-seq-call-loop channels already exist"
                 {:async-channels async-channels})))

(defn stop-async-seq-call-loop!
  []
  (a/>!! (:seq-call-chan @async-channels) {:shutdown-loop? true})
  (a/close! (:seq-call-loop @async-channels))
  (a/close! (:seq-call-chan @async-channels))
  (swap! async-channels dissoc :seq-call-loop :seq-call-chan)
  (timbre/info "async-seq-call channels stoppped"))

(defn sequence-call
  "Prevent function calls from happening to close together.
  This is useful for example when calling the SC server with synthdefs that might saturate it if too close together and while things are playing."
  [timeout f]
  (if-let [chan (:seq-call-chan @async-channels)]
    (a/>!! chan {:f f :timeout timeout})
    (timbre/error "`init-async-seq-call-loop!' has not been called"
                  {:async-channels async-channels})))

(comment
  (init-async-seq-call-loop!)
  (stop-async-seq-call-loop!)

  (doseq [x (range 5)]
    (sequence-call 100 #(println x))))
