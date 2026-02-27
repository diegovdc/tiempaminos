(ns tieminos.tierra-mar.exploracion-2
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :refer [deg->freq]]
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn
                                                       plug*]]
   [tieminos.seq-utils.core :refer [** ++ lin rainseq ret]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

(defn map-outs
  "Given a sequence of outs, map a signal array to each out."
  [out-offset outs-seq sig]
  (if (and (sequential? outs-seq)
           (sequential? sig))
    (do
      (timbre/info "RUNNING MAP OUTS")
      (map (fn [out sig]
             (o/out:ar (oc/+ out out-offset) sig))
           outs-seq
           sig))
    (do (timbre/warn "[map-outs] `outs-seq` & `sig` are not both vectors. Resorting to default output method for current synth variation.")
        (o/out outs-seq sig))))

(defn +outs1
  [params]
  (assoc params
         :out-offset 0
         :ugen/outs (plug* [:out-offset :outs]
                           '((fn [sig] (map-outs out-offset outs sig))))
         :outs [0 1 2 3]))

(defplug outs1
  {:out-offset 0
   :ugen/outs '((fn [sig] (map-outs out-offset outs sig)))
   :outs [0 1]})

(defplug panner
  #{:outs :a :r}
  {:width 1.5
   :orientation 0
   :pan-dur-ratio 1
   :ugen/pan '((fn [sig]
                 (let [dur (+ a r)]
                   (o/pan-az (count outs)
                             sig
                             (o/line 0
                                     (* 2 (/ (dec (count outs))
                                             (count outs)))
                                     (* pan-dur-ratio dur)
                                     :action o/NO-ACTION)
                             :width (o/env-gen (o/envelope [width width 1 0.5]
                                                           [(* 0.8 dur)
                                                            (* 0.1 dur)
                                                            (* 0.1 dur)])
                                               :action o/NO-ACTION)
                             :orientation orientation))))})

(make-synth-fn
 'plucky
 (-> {:freq 440
      :amp 0.5
      :pluck-pos 0.5
      :c1 1
      :c3 20
      :a 0.01
      :r 1}
     (panner)
     (outs1))
 '(-> (let [env   (o/envelope [0 1 1 0] [0.001 0.006 0.0005] [5 -5 -8])
            inp   (* amp (o/lf-clip-noise 2000) (o/env-gen env))]
        (-> (o/dwg-plucked freq amp 1 pluck-pos c1 c3 inp)
            (* (o/env-gen (o/env-perc a r) :action o/FREE))))
      :ugen/pan
      :ugen/outs)
 {:reset? true})

(make-synth-fn
 'bowy
 (-> {:freq 440
      :velb 0.5
      :force 1
      :amp 0.5
      :bow-pos 0.14
      :c1 1
      :c3 3
      :a 0.01
      :r 1
      :impz 0.55
      :inharm 2}

     (panner)
     (outs1))
 '(-> (o/dwg-bowed freq velb force 1 bow-pos r c1 c3 impz inharm)
      (* amp (o/env-gen (o/env-perc a r) :action o/FREE))
      (o/lpf 1200)
      :ugen/pan
      :ugen/outs)
 {:reset? true})

(def scale (:scale (cps/make 2 [1 3 7 9])))
(def scale2 (:scale (cps/make 2 [1 3 9 11])))

(comment
  (o/stop)
  (plucky {:out-offset 20
           :r 10
           :outs (range 44)})

  (rain.v2/stop)
  (rain.v2/ref-rain
   :id ::a
   :durs [3 3 2 3 2]
   :ratio 1/2
   :on-event (rain.v2/on-event
              (let [out-i (rainseq (range 44))]
                #_(plucky {:freq (deg->freq scale 200 (rainseq [[0 0 -6 -9 -12 6]
                                                                [1 2]
                                                                [3 -2 -5]
                                                                [10 -5]]))
                           :amp (rainseq (** 1/8 [1 2 3 (lin 4 1) 2 3 1]))
                           :c1 0.99
                           :c3 (rrange 20 30)
                           :a (rrange 0.01 0.2)
                           :r (rrange 3 4)
                           :outs (->> (rainseq {(ret 1 0) 4
                                                (ret 4 3 2 1) 1
                                                (apply ret (range 19)) 1})
                                      (map #(-> %
                                                (+ out-i)
                                                (* (rand-nth [-1 1]))
                                                (mod 44)
                                                (+ 20))))})

                #_(bowy {:freq (deg->freq scale 200 (rainseq [(lin 0 0 -6 -9 -12)
                                                              [1 2]
                                                              [3 -2 -5]
                                                              [10 -5]]))
                         :amp (rainseq (** 1/2 [1 2 3 (lin 4 1) 2 3 1]))
                         :c1 (rrange 0.2 1)
                         :c3 (rrange 1 3)
                         :inharm 0.3
                         :bow-pos 0.9
                         :a (rrange 2 4)
                         :r (rrange 3 8)
                         :outs (->> (rainseq {(ret 1 0) 4
                                              (ret 4 3 2 1) 1})
                                    (map #(-> %
                                              (+ out-i 22)
                                              (* (rand-nth [-1 1]))
                                              (mod 44)
                                              (+ 20))))}))))
  (doseq [i [0 2 3 5]]
    (println (rainseq {#(rrange 1 4) 1})))
  (rain.v2/stop)

  (do
    (def transp (repcat [88 0]
                        [44 [0 0 2]]
                        [88 0]
                        [44 [0 0 5 -6 5 5]]
                        [88 [7]]
                        [88 [8]]
                        [88 [9]]
                        [88 [10]]
                        #_[88 0]
                        #_[88 3]
                        #_[88 1]
                        #_[88 -1]))

    (rain.v2/ref-rain
     :id ::b
     :durs [1]
     :ratio 1/4
     :on-event (rain.v2/on-event
                (let [config {:freq (deg->freq scale2 400 (rainseq (++ transp [12 6 12 6 0] [0 3 (lin 6 5)])))
                              :pluck-pos (rrange 0.3 0.6)
                              :amp (rainseq (** 1/16 1/2 [1 2 3 4 2 3 1]))
                              :c1 0.1
                              :c3 (rrange 2 30)
                              :a (rrange 0.01 0.1)
                              :r (rainseq {(rrange 1 2) 4
                                           5 1
                                           10 1})
                              :width 3
                              :pan-dur-ratio (rainseq (lin :id/pdr 1 0.5 0.3 0.8 1 0.2))
                              :outs (let [starting-out (rainseq (apply lin :id/a (range 44)))]
                                      (->> (rainseq {(ret 1 0) 4
                                                     (ret 0 1 2 3 4) 1})
                                           (map #(-> %
                                                     (+ starting-out)
                                                     (mod 44)
                                                     (+ 20)))))}]
                  #_(when (#{0  3} (mod i 5))
                      (plucky (-> config
                                  (update :amp * 1/8))))

                  #_(when (#{0  4} (mod i 7))
                      (plucky (-> config
                                  (assoc

                                   :freq (deg->freq (rand-nth [scale2 scale]) 200 (rainseq #_(repcat [3 [(range 6)
                                                                                                         (range -6 12)]]
                                                                                                     [3 (range 6)]
                                                                                                     #_[1 (range 7)])
                                                                                   (++ transp
                                                                                       [6 0 6 0 -6 0 0]
                                                                                       (mapcat #(repeat 2 %) (range 6))
                                                                                       (lin (lin -6 0) [3 7 3] (lin 6 5)))))
                                   :pluck-pos (rrange 0.01 0.3)
                                   :c1 0.5
                                   :c3 (rrange 2 30)
                                   :width 2
                                   :pan-dur-ratio (rainseq (lin :id/pdr 1 0.5 0.3 0.8 1 0.2))
                                   :outs (let [starting-out (rainseq (apply lin :id/b (range 44)))]
                                           (->> (rainseq {(ret 0 1) 4
                                                          (apply ret (range 4)) 1
                                                          (apply ret (range 9)) 1})
                                                (map #(-> %
                                                          (+ starting-out)
                                                          (mod 44)
                                                          (+ 20 44))))))
                                  (update :amp * 1/8))))
                  (when (#{0 6} (mod i 9))
                    (let [range* (range (inc (rand-int 4)))
                          scale* (rand-nth [scale2 scale])]
                      (doseq [n range*]
                        (bowy (-> config
                                  (assoc
                                   :freq (deg->freq scale* (* (inc n) (rand-nth [200 400])) (rainseq (++ transp
                                                                                                         (lin (lin -6 0) [3 7 2 3] 4 (lin 6 5)))))
                                   :a (rainseq {#(rrange 2 4) 8
                                                #(rrange 0.1 0.2) 1})
                                   :r (rrange  4 10)
                                   :width (rrange 5 20)
                                   :pan-dur-ratio (rainseq (lin :id/pdr 1 0.5 0.3 0.8 1 0.2))
                                   :outs (let [starting-out (rainseq (apply lin :id/a (range 44)))]
                                           (->> (rainseq {(ret 1 0) 4
                                                          (ret 4 3 2 1) 1
                                                          (apply ret (range 9)) 4
                                                          (apply ret (range 19)) 4
                                                          (apply ret (range 0 -9 -1)) 1
                                                          (apply ret (range  0 -19 -1)) 1
                                                          (apply ret (range  0 -44 -1)) 1
                                                          (apply ret (range 44)) 1})
                                                (map #(-> %
                                                          (+ (* 6 n) starting-out)
                                                          (mod 44)
                                                          (+ 20 44 44))))))
                                  (update :amp * 6 (/ 1 (count range*)))))))))))))

(make-synth-fn
 'panny
 (-> {:freq 200
      :amp 0.5
      :dur 2
      :pan-dur-amp 1
      :orientation 0
      :width 3}
     +outs1)
 '(-> freq

      o/saw
      (o/moog-ladder (* 3/4 freq) 0.7)
      #_(* (o/sin-osc (* (/ freq (*  64 4 8)) dur)))
      (#(o/pan-az (count outs) %
                  (o/line 0
                          (* 2 (/ (dec (count outs))
                                  (count outs)))
                          (* pan-dur-amp dur)
                          :action o/NO-ACTION)
                  :width width
                  ;; :width (o/env-gen (o/envelope [width width 2 0]
                  ;;                               [(* 0.7 dur)
                  ;;                                (* 0.1 dur)
                  ;;                                (* 0.2 dur)]))
                  :orientation orientation))
      #_(o/free-verb 0.5 2)
      (* amp
         (o/amp-comp freq)
         (o/env-gen #_(o/env-perc 0.01 dur)
          (o/envelope [0 1 0.7 0]
                      [0.1 (* 0.3 dur) (- (* 0.7 dur) 0.1)])
                    :action o/FREE))
      :ugen/outs)
 {:reset? true})
(o/stop)
(comment
  (panny
   {:out-offset (+ 32)
    :dur 10
    :freq (* 200 16)
    :width 8
    :outs
    [32 33]
    #_[12 13]
    #_(concat (range 44)
              (range 44)
              (range 44))})
  (rain.v2/stop)
  (def eiko (:scale (cps/make 3 [1 3 5 7 9 11])))
  (rain.v2/ref-rain
   :id :test-spiral                    ;; 44ch
   :durs [3 2 2]
   :tempo 120
   :ratio 1/4
   :on-event (rain.v2/on-event
              (panny
               (let [deg (rainseq (range 0 44) #_(++  [0 0  #_(lin 3 0)  0 0 7]
                                                      (range 10)
                                                      [0 0 0 #_(lin 0 0 13 14 16 17) 0 0 0 0 0  2]))
                     outs (map #(max (- 44 deg %) 0)
                               (range 2 8))]
                 (println deg outs)
                 {:out-offset (+ 32)
                  :dur (* dur 5 3)
                  :pan-dur-amp 1
                  :freq (deg->freq scale2 200 deg)
                  :width 1
                  :outs outs #_(map #(+ % (rainseq (++ 0 (mirror #_(range 0 22)
                                                          (range 12 13)
                                                                 #_(range 32 34)))))
                                    [0 1])
                  #_(concat (range 44)
                            (range 44)
                            (range 44))}))))
  (rain.v2/ref-rain
   :id :test-arcs
   :durs [1]
   :tempo 120
   :ratio 1/2
   :on-event (rain.v2/on-event
              (panny
               (let [deg (rainseq [22 15])
                     outs (reverse (rainseq (let [offset 1]
                                              [(apply ret (reverse (range (- 4 offset) (+ 6 offset))))
                                               #_(apply ret (reverse (range (- 24 offset) (+ 26 offset))))])))]
                 {:out-offset (+ 32)
                  :dur (* dur 4)
                  :pan-dur-amp 1
                  :freq (deg->freq scale2 200 deg)
                  :width 2
                  :outs outs #_(map #(+ % (rainseq (++ 0 (mirror #_(range 0 22)
                                                          (range 12 13)
                                                                 #_(range 32 34)))))
                                    [0 1])
                  #_(concat (range 44)
                            (range 44)
                            (range 44))})))))

(comment
  ;; pruebas en el LIMME
  (make-synth-fn
   'panny
   (-> {:freq 200
        :amp 0.5
        :dur 2
        :pan-dur-amp 1
        :orientation 0
        :width 3}
       +outs1)
   '(-> freq

        o/saw
        (o/moog-ladder (* 3/4 freq) 0.7)
        #_(* (o/sin-osc (* (/ freq (*  64 4 8)) dur)))
        (#(o/pan-az (count outs) %
                    (o/line 0
                            (* 2 (/ (dec (count outs))
                                    (count outs)))
                            (* pan-dur-amp dur)
                            :action o/NO-ACTION)
                    :width width
                     ;; :width (o/env-gen (o/envelope [width width 2 0]
                     ;;                               [(* 0.7 dur)
                     ;;                                (* 0.1 dur)
                     ;;                                (* 0.2 dur)]))
                    :orientation orientation))
        #_(o/free-verb 0.5 2)
        (* amp
           (o/amp-comp freq)
           (o/env-gen #_(o/env-perc 0.01 dur)
            (o/envelope [0 1 0.7 0]
                        [0.1 (* 0.3 dur) (- (* 0.7 dur) 0.1)])
                      :action o/FREE))
        :ugen/outs)
   {:reset? true})
  (o/stop)
  (comment
    (panny
     {:out-offset (+ 32)
      :dur 10
      :freq (* 200 16)
      :width 8
      :outs
      [32 33]
      #_[12 13]
      #_(concat (range 44)
                (range 44)
                (range 44))})
    (rain.v2/stop)
    (def eiko (:scale (cps/make 3 [1 3 5 7 9 11])))
    (rain.v2/ref-rain
     :id :test-spiral                    ;; 44ch
     :durs [3 2 2]
     :tempo 120
     :ratio 1/4
     :on-event (rain.v2/on-event
                (panny
                 (let [deg (rainseq (range 0 44) #_(++  [0 0  #_(lin 3 0)  0 0 7]
                                                        (range 10)
                                                        [0 0 0 #_(lin 0 0 13 14 16 17) 0 0 0 0 0  2]))
                       outs (map #(max (- 44 deg %) 0)
                                 (range 2 8))]
                   (println deg outs)
                   {:out-offset (+ 32)
                    :dur (* dur 5 3)
                    :pan-dur-amp 1
                    :freq (deg->freq scale2 200 deg)
                    :width 1
                    :outs outs #_(map #(+ % (rainseq (++ 0 (mirror #_(range 0 22)
                                                            (range 12 13)
                                                                   #_(range 32 34)))))
                                      [0 1])
                    #_(concat (range 44)
                              (range 44)
                              (range 44))}))))
    (rain.v2/ref-rain
     :id :test-arcs
     :durs [1]
     :tempo 120
     :ratio 1/2
     :on-event (rain.v2/on-event
                (panny
                 (let [deg (rainseq [22 15])
                       outs (reverse (rainseq (let [offset 1]
                                                [(apply ret (reverse (range (- 4 offset) (+ 6 offset))))
                                                 #_(apply ret (reverse (range (- 24 offset) (+ 26 offset))))])))]
                   {:out-offset (+ 32)
                    :dur (* dur 4)
                    :pan-dur-amp 1
                    :freq (deg->freq scale2 200 deg)
                    :width 2
                    :outs outs #_(map #(+ % (rainseq (++ 0 (mirror #_(range 0 22)
                                                            (range 12 13)
                                                                   #_(range 32 34)))))
                                      [0 1])
                    #_(concat (range 44)
                              (range 44)
                              (range 44))}))))))
