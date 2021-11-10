(ns erv-fib-synth.compositions.garden-earth.main
  (:require [erv.utils.core :as u]
            [erv-fib-synth.osc.reaper :as reaper]
            [taoensso.timbre :as timbre]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [erv-fib-synth.compositions.garden-earth.base :refer [stop log-with-msg log]]
            [erv-fib-synth.compositions.garden-earth.misterioso
             :refer [misterioso]]
            [erv-fib-synth.compositions.garden-earth.claro
             :refer [claro]]
            [erv-fib-synth.compositions.garden-earth.oscuro :as oscuro]))

(declare seconds->dur garden-earth)

(def bpm 900)
(def sections
  ;; Extender el principio para que entre la flauta con más calma y pueda desarrollar en esa armonía, luego de manera relativamente apresurada hacer los cambios armónicos de la segunda parte de Claro hacia algo más disonante, luego ir a #{1 3} y luego terminar la sección
  [[20 #(do #_(reaper/rec)
            (claro :set* #{11 7} :moments [:oscuro] :id :claro
                   :vel-amp 1 :offset 3))]
   [10 #(claro :set* #{11 7} :moments [:oscuro] :id :claro
               :vel-amp 1 :offset 2)]
   [8 #(claro :set* #{9 7} :moments [:oscuro] :id :claro
              :vel-amp 1 :offset 2 :tempo-ratio 7/9)]
   [8 #(claro :set* #{11 7} :moments [:oscuro] :id :claro
              :vel-amp 1 :offset 2 :tempo-ratio 7/9)]
   [10 #(claro :set* #{9 7} :moments [:oscuro] :id :claro
               :vel-amp 1 :offset 3 :tempo-ratio 9/7)]
   [8 #(claro :set* #{9 7} :moments [:oscuro] :id :claro
              :vel-amp 1 :offset 1)]
   ;; 11 9
   [15 #(claro :set* #{11 9} :moments [:oscuro] :id :claro
               :vel-amp 0.7) :tempo-ratio 11/9]
   [8 #(claro :set* #{11 7} :moments [:claro] :id :claro
              :vel-amp 1.3 :offset 0) :tempo-ratio 7/11]
   [10 #(claro :set* #{7 9} :moments [:claro] :id :claro
               :vel-amp 1.4 :offset 2 :tempo-ratio 7/11)]
   [10 #(claro :set* #{11 9} :moments [:oscuro] :id :claro
               :vel-amp 1 :offset -3 :tempo-ratio 11/7 :durs [15000 10000])]

   [8 #(claro :set* #{9 7} :moments [:oscuro] :id :claro
              :vel-amp 1 :offset 2)]
   [8 #(claro :set* #{11 9} :moments [:claro] :id :claro
              :vel-amp 1.1) :offset 1]
   [10 #(claro :set* #{1 9} :moments [:oscuro] :id :claro
               :vel-amp 0.7 :offset 2)]

   [8 (fn [] :nothing)]
   [10 #(claro :set* #{1 3} :moments [:claro] :id :claro
               :vel-amp 1.1 :offset 3
               :durs [10000 5000])]
   [5 #(claro :set* #{11 3} :moments [:claro] :id :claro
              :vel-amp 1.1 :offset 3
              :durs [20000 15000])]
   #_[5 #(claro :set* #{3} :moments [:claro :oscuro] :id :claro
                :vel-amp 2 :tempo-ratio 11/13 :offset -5
                :durs [15000 10000])]
   [5 #(claro :set* #{7 3} :moments [:claro] :id :claro
              :vel-amp 0.5 :tempo-ratio 7/13 :offset -2
              :durs [8000 10000])]
   [8 #(claro :set* #{11 3} :moments [:claro]
              :id :claro :vel-amp 0.2 :tempo-ratio 5/13 :offset 5
              :durs [5000])]
   [5 #(claro :set* #{1 3} :moments [:claro] :id :claro
              :vel-amp 1.1 :offset 3
              :durs [10000 5000])]
   [10 #(do (stop :claro) (timbre/info "Ends Claro"))]
   [40 #(misterioso :id :misterioso)]
   #_[20 #(do (oscuro/sets-3-11-and-3-5 :moments [:claro] :vel-amp 1)
              (stop :misterioso))]
   [1 #(do (stop) (reaper/stop) (timbre/info "The end"))]])


(comment
  (garden-earth sections :start-index 0)
  (stop)
  (reaper/init))

(comment
  (stop)
  ;;; WIP  esquema de secuencia
  (claro :moment :oscuro
         :id :claro
         :vel-amp 1
         :offset 3
         :set* #{11 7})
  (claro :moment :oscuro
         :id :claro
         :vel-amp 1
         :offset 3
         :set* #{9 7})
  (claro :moment :oscuro
         :id :claro
         :vel-amp 0.7
         :set* #{11 9})
  (claro :moment :oscuro
         :id :claro
         :vel-amp 0.7
         :set* #{1 9})
  (claro :moment :oscuro
         :id :claro
         :vel-amp 0.7
         :set* #{1 3})
  ;; simultaneas las siguientes 3
  (claro :moment :oscuro
         :id :claro
         :vel-amp 0.7
         :offset 1
         :set* #{7 9})
  (claro :moment :claro
         :id :claro-2
         :vel-amp 0.7
         :offset 1
         :set* #{11 9})
  (claro :moment :claro
         :id :claro-3
         :vel-amp 0.7
         :offset -2
         :set* #{11 7}))

(defn seconds->dur [secs bpm] (* secs (/ bpm 60)))

(defn garden-earth [sections & {:keys [start-index] :or {start-index 0}}]
  (let [sections*   (map #(update-in % [0] seconds->dur bpm)
                         sections)
        durs (map first sections*)]
    (ref-rain :id ::main
              :durs durs
              :loop? false
              :tempo bpm
              :on-event (on-event ((-> sections
                                       (nth (+ start-index index))
                                       second))))))
