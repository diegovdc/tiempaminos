(ns tieminos.synths.wavetable
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [overtone.core :as o]
            [overtone.sc.ugen-collide-list :as oc]))

(str (System/getProperty "user.dir") "/samples/AKWF-wavetable/")

(def akwf-samples-path "./samples/AKWF-wavetable/")

(def ^:private akwf-directories
  (->> (io/file akwf-samples-path)
       file-seq
       (drop 1)
       (filter #(.isDirectory %))
       (map #(.getName %))))

(defn load-akwf-samples!
  []
  (doall (->> akwf-directories
              (mapv (fn [dir]
                      [(keyword (str/replace dir #"AKWF_" ""))
                       (->> (io/file (str akwf-samples-path dir))
                            file-seq
                            (remove #(.isDirectory %))
                            (map #(.getPath %))
                            sort
                            (map o/load-sample))]))
              (into {}))))

(comment
  (def akwf (load-akwf-samples!))
  (-> akwf)
  (o/sample-player (-> akwf :epiano (nth 22))))

(defmacro default-env [dur a s d r]
  `(o/env-gen (o/envelope [0 1 ~s ~s 0] [~a ~d (oc/max 0 (oc/- ~dur ~a ~d ~r)) ~r])
              :time-scale ~dur
              :action o/FREE))

(defmacro default-filter-env [freq dur fl1 fl2 fl3 fl4 ft1 ft2 ft3]
  '(o/env-gen (o/envelope (map #(* freq %) [fl1 fl2 fl3 fl4]) [ft1 ft2 ft3]) :time-scale dur))

(defmacro chorus-osc [freq beats phase-lfo]
  '(+ (/ (o/osc buf freq (o/lf-noise1 phase-lfo)) 5)
      (/ (o/osc buf (+ freq beats) (o/lf-noise1 phase-lfo)) 5)
      (/ (o/osc buf (+ freq (/ beats 2)) (o/lf-noise1 phase-lfo)) 5)
      (/ (o/osc buf (+ freq (* -1 (/ beats 2))) (o/lf-noise1 phase-lfo)) 5)
      (/ (o/osc buf (+ freq (* -1 beats)) (o/lf-noise1 phase-lfo)) 5)))

(comment
  (defn)
  (defn scale-down [x] (oc// x 100))

  (o/defsynth multy [freq 200
                     amp 0.3]
    (o/lin-lin) (let [mod-freq (scale-down freq)]
                  (o/out 0 (* amp (o/sin-osc freq)
                              (o/sin-osc mod-freq)))))
  (o/defsynth multy2 [freq 200
                      amp 0.3]
    (let [mod-freq (scale-down freq)]
      (o/out 0 (* amp (o/sin-osc freq)
                  (o/sin-osc (/ 200 100))))))
  (multy)
  (multy 500)
  (multy2 500)
  (multy2)
  (o/stop))

(o/defsynth pad
  [buf 0
   freq 300
   amp 1
   a 1
   d 0.1
   s 1
   r 1
   dur 1
   gate 1
   pan 0
   phase-lfo 1
   beats 0                              ; for chorus
   out 0]
  (o/out out
         (-> (chorus-osc freq beats phase-lfo)
             (o/pan2 pan)
             (* amp (default-env dur a s d r)))))

(comment
  (def p (pad :buf (-> akwf :epiano (nth 6)) :d 5 :dur 2 :beats 0 :phase-lfo 1))
  (def p (lpfpad :buf (-> akwf :epiano (nth 0)) :d 5 :dur 2 :beats 1 :phase-lfo 1))
  (def p (hpfpad :buf (-> akwf :epiano (nth 0)) :d 5 :dur 2 :beats 0.3 :phase-lfo 1)))

(o/defsynth lpfpad
  [buf 0
   freq 300
   amp 1
   a 1
   d 0.1
   s 1
   r 1
   dur 1
   gate 1
   pan 0
   phase-lfo 1
   beats 0                              ; for chorus
   out 0
   ;; filter (rq, levels and times)
   rq 0.4
   fl1 4
   fl2 6
   fl3 2
   fl4 3
   ft1 1
   ft2 1
   ft3 1]
  (o/out out
         (-> (chorus-osc freq beats phase-lfo)
             (o/rlpf (default-filter-env freq dur fl1 fl2 fl3 fl4 ft1 ft2 ft3) rq)
             (o/pan2 pan)
             (* amp (default-env dur a s d r)))))
(o/defsynth hpfpad
  [buf 0
   freq 300
   amp 1
   a 1
   d 0.1
   s 1
   r 1
   dur 1
   gate 1
   pan 0
   phase-lfo 1
   beats 0                              ; for chorus
   out 0
   ;; filter (rq, levels and times)
   rq 0.4
   fl1 4
   fl2 6
   fl3 2
   fl4 3
   ft1 1
   ft2 1
   ft3 1]
  (o/out out
         (-> (chorus-osc freq beats phase-lfo)
             (o/rhpf (default-filter-env freq dur fl1 fl2 fl3 fl4 ft1 ft2 ft3) rq)
             (o/pan2 pan)
             (* amp (default-env dur a s d r)))))

(comment
  "SynthDef(\\wvpad, {
	| bufPos=0.1, phase=1, min=40, max= 50, freq=300, amp=1,
	att=1, dec=0.1, sus=1, rel=1, gate=1, pan=0.5, out=0 |
	var range, osc, sig, env;
	range= ~range.(min, max);
	osc= SinOsc.ar(bufPos, phase, range.multi, range.addi);
	sig= VOsc.ar(osc, freq, 0, amp);
	env= EnvGen.kr(Env.adsr(att, dec, sus, rel),gate, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env))
}).add";
  )
#_(defn chrousize [sweeper freq]
    (let [beats 0
          step-size (oc// beats 4)]
      (o/mix (->> (range 0 (oc/+ beats step-size) step-size)
                  (mapv (fn [i]
                          (oc/* 1 (o/v-osc sweeper
                                           (oc/+ freq (- i (oc// beats 2)))
                                           0))))))))

(defn chrousize [sweeper freq beats]
  (oc/* 1/5 (oc/+ (o/v-osc sweeper freq 0)
                  (o/v-osc sweeper (oc/+ freq (oc/* beats 1/4)) 0)
                  (o/v-osc sweeper (oc/+ freq (oc/* beats -1/4)) 0)
                  (o/v-osc sweeper (oc/+ freq (oc/* beats 1/2)) 0)
                  (o/v-osc sweeper (oc/+ freq (oc/* beats -1/2)) 0))))

(defmacro sin-vosc [freq sweep-freq phase min max beats]
  `(let [osc# (o/lin-lin (o/sin-osc ~sweep-freq ~phase)
                         -1 1 ~min ~max)]
     (chrousize osc# ~freq ~beats)
     #_(o/mix (->> (range 5)
                   (mapv (fn [i] (* 1/5 (o/v-osc osc# (+ ~freq 0.5) 0))))))))
(comment

  (macroexpand-1 '(sin-vosc 500 1 2 3 4 0.5))

  (sin-vosc 500 1 2 3 4 0.5))

(o/defsynth vpad
  [sweep-freq 0.1
   buf 0
   buf-range 0
   freq 300
   amp 1
   a 1
   d 0.1
   s 1
   r 1
   dur 1
   gate 1
   pan 0
   phase 1
   beats 0                              ; for chorus
   out 0
   beats 0.5
   ;; filter (rq, levels and times)
   rq 0.4
   fl1 4
   fl2 6
   fl3 2
   fl4 3
   ft1 1
   ft2 1
   ft3 1]
  (let [osc (o/lin-lin (o/sin-osc sweep-freq phase)
                       -1 1 buf buf-range)]
    (chrousize osc freq beats)

    (o/out out
           (-> (sin-vosc freq sweep-freq phase buf buf-range beats)
               #_(o/rhpf (default-filter-env freq dur fl1 fl2 fl3 fl4 ft1 ft2 ft3) rq)
               #_(o/rlpf 24000 0.01)
               (o/pan2 pan)
               (* amp (o/env-gen (o/envelope [0 1 1 0] [a (max 0 (- dur a r)) r])
                                 :time-scale (/ dur (+ a d r dur))
                                 :action o/FREE))))))

(comment
  (o/stop)
  (vpad 0.1
        :dur 2
        :r 20
        :phase 1
        :a 0.001
        :freq 500
        :beats 10
        :buf (-> akwf :epiano first)
        :buf-range 50)

  (-> akwf :epiano))

