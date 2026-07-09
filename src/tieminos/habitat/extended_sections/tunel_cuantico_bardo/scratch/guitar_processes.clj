(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.scratch.guitar-processes
  (:require
   [overtone.core :as o]
   [tieminos.blackhole :as bh]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.overtone-extensions :as oe]))

(comment

  (do
    ;; RM
    #_(oe/defsynth posty
        [in 0
         out 0]
        (let [sig (o/in in 1)
              [freq has-freq?] (o/pitch sig :exec-freq 1)]
          #_(o/poll:ar (o/impulse 0.5) (o/pitch sig) has-freq?)
          (o/out out (-> #_sig
                      (+
                       (+ (* 1 has-freq?
                             sig
                             (o/saw (* 7/4 freq)))))
                         (o/free-verb 0.4 1)
                         (o/pan2)))))

    (oe/defsynth posty
      [in 0
       pitch-follower-freq 1
       out 0]
      (let [sig (o/in in 1)
            [freq has-freq?] (o/pitch sig :exec-freq pitch-follower-freq)
            amp* (o/amplitude sig)
            ratio 2/9]
        (o/poll:ar (o/impulse 0.5) (o/dc) freq has-freq?)
        #_(o/out out (-> (o/range-lin sig (* freq 2) (* freq -2))
                         o/sin-osc))
        (o/out out (-> #_sig
                    (+ (* 2 sig)
                       (-> (*  (o/range-lin  (*  sig) (* freq ratio) #_(* freq (/ 1 ratio))))
                           (o/sin-osc-fb (* 2 amp*))
                           (* (o/lag2 (* 4 amp*) 1))))
                       (o/moog-ladder (o/clip (* 9 freq) 20 10000) 0.8)
                       (* 2)
                       (o/distort)
                       (o/comb-l 1 (/ 1 (o/lag2 freq 0.01) 4) 2)
                       #_(o/hpf 600)
                       #_(o/pitch-shift 0.2 [1 3/2 7/4])
                       #_(o/mix)
                       (o/free-verb 0.5 0.7)
                       (o/pan2)
                       (o/leak-dc)

                       (* 8)))))
    (when (o/node-active? t)
      (o/kill t))

    (def t (posty {:in (-> @inputs :guitar :bus)
                   :out (bh/bus 14)})))

  (o/ctl t :pitch-follower-freq 0.00001))

;;;;;;;;;;;;;;;;;;
;; * NOTES
;;;;;;;;;;;;;;;;;;
(comment

  ;; first

  (let [sig (o/in in 1)

        ;; :exec-freq can be useful for rhtymic/glitchy stuff... in fact almost certainly there will be glitches... condsider using `latch` to control the pitch changes in some of the synth variations, as a freeze.
        ;; :median controls the amount of variation/glitchyness so also very useful, 1 will work well for the original exploration with FM#2 + comb + hpf
        [freq has-freq?] (o/pitch sig :min-freq pitch-follower-freq :exec-freq pitch-follower-freq :median 1)

        ;; this is useful most of the time, but for the original variation, if this is not used it will work very nicely 
        amp* (o/amplitude sig)

        ;; Very useful speciallly with FM #0 and #1; FM #2 requires a higher ratio like 10+
        ratio 2/9]
    #_...)

  ;; * Main sig
  ;; add dry/wet
  (+ (* 2 sig)
     (-> (*  (o/range-lin  (*  sig) (* freq ratio) (* freq (/ 1 ratio))))
         ;; other variations:
         ;; 1. (o/range-lin  (*  sig) (* freq ratio) freq) ;; this one makes it one sided, kind of harmonious
         ;; 2. (o/range-lin  (*  sig) (* freq ratio) (* freq -1 ratio))  ;; this was is very noisy, and creates lots of low frequencies but sounds good with the comb config below (include the hpf:600 filter)
         ;; 3. (* 1 has-freq? sig (o/saw (* 7/4 freq))) ;; The RM version at the top
         (o/sin-osc-fb (* 2 amp*))
         (* (o/lag2 (* 4 amp*) 1))))

;; filter with amp; to define if this is the right spot
  (o/moog-ladder (o/clip (* 9 freq) 20 10000) 0.8)
  (* 2)

  ;; distort to blend in
  (o/distort)

  ;; comb, this is kind of cool
  (o/comb-l 1 (* (/ 1 (o/lag2 freq 0.01))
                 1/4 ;; two octaves above, would be nice to support a vector for harmony
                 )
            0.3)

  ;; filter... maybe a lpf also? this should be a filter section
  (o/hpf 600)

  ;; ps section; add dry/wet... this works well with FM #2 plus the comb
  (o/pitch-shift 0.2 [1 3/2 7/4])
  (o/mix)

  ;; rev of course, but post panner, it would be more interesting... but maybe later.
  (o/free-verb 0.5 0.7)

  ;; end
  (o/leak-dc)
  (o/pan2) ;; of course other panning options should be used...
  ;; TODO: if the panner is at the begining the we could have some multifx thing going on... but probably later.
  )

(comment
  (defmacro maquote
    [body]
    (list 'quote body))
  (macroexpand-1 '(maquote (o/sin-osc)))

  (defplug amp-follower
    {:ugen/amp-follower (fn [sig] (o/amplitude sig))})

  (make-synth-fn
   'cosillos
   {:in 0
    :pitch-follower-freq 1
    :fm-ratio 10
    :out 0}

   (maquote
    (let [sig (o/in in 1)
          [freq _has-freq?] (o/pitch sig :exec-freq pitch-follower-freq)
          amp* (:ugen/amp-follower sig)]
      #_(o/poll:ar (o/impulse 0.5) (o/pitch sig) has-freq?)
      #_(o/out out (-> (o/range-lin sig (* freq 2) (* freq -2))
                       o/sin-osc))
      (o/out out (-> #_sig
                  (+ sig
                     (-> (o/range-lin sig (* freq fm-ratio) (* freq -1 fm-ratio))
                         o/sin-osc
                         (* amp*)))
                     #_(o/moog-ladder (o/clip (* 5 freq) 20 10000) 0.5)
                     (o/comb-l 1 (* (/ 1 (o/lag2 freq 0.01)) 1/4) 0.3)
                     (o/hpf 600)
                     (o/pitch-shift 0.2 [1 3/2 7/4])
                     (o/mix)
                     (o/free-verb 0.5 0.7)
                     (o/pan2)
                     (o/leak-dc)
                     #_(* 8)))))))
