(ns tieminos.compositions.7D-percusion-ensamble.dreams.hydra-control
  (:require
   [clojure.core.async :as a]
   [clojure.data.generators :refer [weighted]]
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.utils :refer [cb-interpolate rrange stop-all-interpolators!]]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  (init-osc-server)
  (init))

(declare fade-from-black
         fade-to-black
         to-gray
         to-color
         moment-1
         play-main
         ending
         init)

(def controller-state (atom {:moment-1-called? false
                             :play-main-called? false
                             :end-called? false}))
(defn init-osc-server []
  (habitat-osc/init :port 7777)
  (habitat-osc/responder
    (fn [{:keys [path args] :as msg}]
      (println msg)
      (try
        (case path
          "/fade-from-black" (fade-from-black)
          "/fade-to-black" (fade-to-black 7000)
          "/to-gray" (to-gray)
          "/to-color" (to-color)
          "/moment-1" (if-not (:moment-1-called? @controller-state)
                        (do (moment-1)
                            (swap! controller-state assoc :moment-1-called? true))
                        (timbre/warn "moment-1 already called"))
          "/play-main" (if-not (:play-main-called? @controller-state)
                         (do (play-main)
                             (swap! controller-state assoc :play-main-called? true))
                         (timbre/warn "play-main already called"))
          "/end" (if-not (:end-called? @controller-state)
                   (do (ending)
                       (swap! controller-state assoc :end-called? true))
                   (timbre/warn "end already called"))
          "/to-init" (do (init)
                         (reset! controller-state {})))
        (catch Exception _ (timbre/error "Unknown path" msg))))))

;; Intended to run on linux machineso the following may need to be done
;; Setup of VirMIDI: https://github.com/anton-k/linux-audio-howto/blob/master/doc/os-setup/virtual-midi.md#virtual-midi-1
;;   NOTE: it's ok to use qjack, it seems.
;; Testing: https://hydra.ojack.xyz/?code=JTJGJTJGJTIwWW91JTIwY2FuJTIwZWl0aGVyJTIwdXNlJTIwJTYwJTQwbGF0ZXN0JTYwJTIwb3IlMjBsb2FkJTIwYSUyMHNwZWNpZmljJTIwdmVyc2lvbiUyMHdpdGglMkMlMjBmb3IlMjBleGFtcGxlJTJDJTIwJTYwJTQwMC40LjAlNjAuJTBBYXdhaXQlMjBsb2FkU2NyaXB0KCUwQSUyMCUyMCdodHRwcyUzQSUyRiUyRmNkbi5qc2RlbGl2ci5uZXQlMkZucG0lMkZoeWRyYS1taWRpJTQwbGF0ZXN0JTJGZGlzdCUyRmluZGV4LmpzJyUwQSklMEElMEElMkYlMkYlMjBVc2UlMjBtaWRpJTIwbWVzc2FnZXMlMjBmcm9tJTIwYWxsJTIwY2hhbm5lbHMlMjBvZiUyMGFsbCUyMGlucHV0cy4lMEFhd2FpdCUyMG1pZGkuc3RhcnQoJTdCJTIwY2hhbm5lbCUzQSUyMCcqJyUyQyUyMGlucHV0JTNBJTIwJyonJTIwJTdEKSUwQSUyRiUyRiUyMFNob3clMjBhJTIwc21hbGwlMjBtaWRpJTIwbW9uaXRvciUyMChzaW1pbGFyJTIwdG8lMjBoeWRyYSdzJTIwJTYwYS5zaG93KCklNjApLiUwQW1pZGkuc2hvdygpJTBBJTBBJTJGJTJGJTIwVXNlJTIwYW55JTIwbm90ZSUyMHRvJTIwY29udHJvbCUyMHRoZSUyMHJlZCUyMGFtb3VudCUyMG9mJTIwaHlkcmEncyUyMCU2MHNvbGlkKCklNjAlMjBmdW5jdGlvbi4lMEElMkYlMkZzb2xpZChub3RlKCcqJyklMkMlMjAwJTJDJTIwMSkub3V0KCklMEElMEElMkYlMkYlMjBPciUyQyUyMGlmJTIweW91JTIwYXJlJTIwdXNpbmclMjBhJTIwbWlkaSUyMGNvbnRyb2xsZXIlMjBhbmQlMjBub3QlMjBhJTIwa2V5Ym9hcmQlM0ElMEElMkYlMkYlMjBVc2UlMjBhJTIwY29udHJvbCUyMGNoYW5nZSUyMHZhbHVlJTIwdG8lMjBjb250cm9sJTIwdGhlJTIwcmVkJTIwYW1vdW50LiUwQSUyMHNvbGlkKGNjKDc0KSUyQyUyMDAlMkMlMjAxKS5vdXQoKQ%3D%3D

(def sink (midi/midi-out "VirMIDI"))

(defn fade [{:keys [cc fade-to dur-ms]}]
  (cb-interpolate {:id (keyword "hydra-control" (str "cc" cc))
                   :dur-ms (or dur-ms 7000)
                   :tick-ms 200
                   :init-val 0
                   :target-val fade-to
                   :cb (fn [{:keys [val]}] (midi/midi-control sink cc val))}))
(defn fade-from-black [] (fade {:cc 74 :fade-to 0}))
(defn fade-to-black [dur-ms] (fade {:cc 74 :fade-to 127 :dur-ms dur-ms}))

#_(defn interpolate-ad-envelope
  [{:keys [id tick-ms atk-ms atk-val dcy-ms dcy-val cb]
    :or {tick-ms 100}}]
  (cb-interpolate
   {:id id
    :dur-ms atk-ms
    :tick-ms (min tick-ms atk-ms)
    :init-val 0
    :target-val atk-val
    :cb cb
    :on-end (fn [{:keys [val]}]
              (cb-interpolate {:id id
                               :dur-ms dcy-ms
                               :tick-ms tick-ms
                               :init-val val
                               :target-val dcy-val
                               :cb cb
                               :on-end nil}))}))

(def cc* {:black-fade 74
          :fractal-add 75
          :fractal-hue 76
          :kaleid-mod-amp 77
          :preout-mod 78
          :in-blend 79
          :voronoi-mod 80
          :hept-x-speed 81
          :hept-x-pos 82
          :hept-y-speed 83
          :hept-y-pos 84
          :hept-scale 85
          :hept-repeat 86
          :hept-luma 87
          :out-saturation 88
          :hept-dancer-repeat 89
          :hept-dancer-scroll-y 90
          :hept-dancer-scroll-x 91
          :hept-dancer-add 92})

(let [cc-fns (into {} (map (fn [[k v]]
                             [k (partial midi/midi-control sink v)]) cc*))]
  (defn cc
    "Map of cc keys to midi-contorl-fn call. Only need to take a value from 0-127"
    [k cc-val]
    (when-let [f (cc-fns k)]
      (f cc-val))))

(def cc-interp*
  "Map of cc key to interpolation funciton that takes a `:target-val` and `:dur-ms`"
  (->> cc*
       (map
        (fn [[k cc-num]]
          [k (fn [target-val dur-ms tick-ms]
               (let [prev-val (atom 0)]
                 (cb-interpolate
                  {:id         (keyword "hydra-control" (str "cc" cc-num "-" (name k)))
                   :dur-ms     dur-ms
                   :tick-ms    tick-ms
                   :init-val   0 ;; TODO add default init val
                   :target-val target-val
                   :cb         (fn [{:keys [val]}]
                                 (let [new-val (int val)]
                                   #_(when-not (= new-val @prev-val)
                                       (println (int val))
                                       (reset! prev-val new-val))
                                   (midi/midi-control sink cc-num (int val))))
                   ;; to ensure the last message got received, seems like hydra drops a lot
                   #_#_:on-end (fn [] (a/go
                                        (a/<! (a/timeout (rrange 800 1000)))
                                        (midi/midi-control sink cc-num target-val)))})))]))
       (into {})))

(defn cc-interp
  ([cc-key target-val dur-ms] (cc-interp cc-key target-val dur-ms 100))
  ([cc-key target-val dur-ms tick-ms]
   (if-let [cc-fn (cc-interp* cc-key)]
     (cc-fn target-val dur-ms tick-ms)
     (timbre/warn "Function for cc-key not found:"  cc-key))))

(defn to-gray [] (cc-interp :out-saturation 0 5000 100))
(defn to-color [] (cc-interp :out-saturation 127 5000 100))

(defn init []
  (gp/stop)
  (stop-all-interpolators!)
  (doseq [[k v] {:fractal-add 0
                 :black-fade 127
                 :out-saturation 0
                 :fractal-hue 0
                 :kaleid-mod-amp 0
                 :preout-mod 0
                 :in-blend 30
                 :voronoi-mod 0
                 :hept-scale 10
                 :hept-repeat 0
                 :hept-luma 127
                 :hept-dancer-add 0}]

    (cc k v))
  )

(comment
  (cc-interp :voronoi-mod 0 2000 200)
  (cc :hept-dancer-add 0 )
  (cc-interp :preout-mod 0 2000 200))

(defn moment-1 []
  (fade-from-black))

(defn moment-2 []
  (timbre/info "Moment 2")
  (to-color))

(defn moment-3 []
  (timbre/info "Moment 3")
  (cc-interp :fractal-add 90 20000 (rrange 400 500))
  (cc-interp :fractal-hue 30 20000 (rrange 400 500)))

(defn moment-4 []
  (timbre/info "Moment 4")
  (doseq [[k v] {:hept-dancer-repeat 127
                 :hept-dancer-add 127}]

    (cc-interp k v (rrange 2000 4000) (rrange 400 500)))
  (cc-interp :kaleid-mod-amp 105 (* 6 60 1000) 857)
  (gp/ref-rain
    :id :hept-pattern
    :durs [1 2]
    :on-event (gp/on-event
                (midi/midi-control
                  sink
                  (cc* :hept-dancer-scroll-x)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc* :hept-dancer-scroll-y)
                  (rand-int 127))

                (midi/midi-control
                  sink
                  (cc* :hept-dancer-repeat)
                  (rand-int 127)))))

(defn moment-5 []
  (timbre/info "Moment 5")
  (cc-interp :fractal-add 50 16666 555)
  (cc-interp :fractal-hue 0 17777 756)
  #_(cc-interp :preout-mod 0 1000 400)
  (cc-interp :preout-mod 9 (* 3 60 1000) 4000)
  (cc-interp :in-blend 10 (* 2 60 1000) 2559)
  #_(cc-interp :voronoi-mod 0 2000 510)
  (cc-interp :voronoi-mod 80 (* 5 60 1000) 2160)
  #_(cc-interp :hept-luma 127 1200 570)
  (cc-interp :hept-luma 0 (* 1 60 1000) 3500)
  (gp/ref-rain
   :id :hept-pattern
   :durs [3 2 2]
   :on-event (gp/on-event
              (midi/midi-control
               sink
               (cc* :hept-x-pos)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-y-pos)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-x-speed)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-y-speed)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-scale)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-repeat)
               (rand-int 127))

              (midi/midi-control
               sink
               (cc* :hept-dancer-add)
               (weighted {0 11
                          30 1
                          40 1
                          80 1/2
                          127 1/2})
               #_(rand-nth [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                               ;; 30 50 80 127
                            ])))))

(defn play-main
  []
  (gp/ref-rain
   :id ::main
   :durs (map #(* % 60 1000) [4 5 6 7])
   :loop? false
   :on-event (gp/on-event
              (try
                (case i
                  0 (moment-2)  ;; 0
                  1 (moment-3)  ;; 7
                  2 (moment-4)  ;; 14
                  3 (moment-5)) ;; 21
                (catch Exception _ nil)))))

(defn ending []
  (fade-to-black 15000))

(comment

  (to-gray)
  (init)
  (moment-1)
  (moment-2)
  (moment-3)
  (moment-4)
  (moment-5)
  (play-main)
  (ending)
  (gp/stop)
  (midi/midi-control sink (cc* :hept-luma) 127)
  (cc-interp :kaleid-mod-amp 105 (* 6  1000) 857)
  (cc :hept-dancer-add 0)
  (cc :kaleid-mod-amp 0))

(comment
  ;;  very cool, explosion-like
  ;; extasis of the 7th dimension
  (midi/midi-control sink (cc* :black-fade) 0)
  (midi/midi-control sink (cc* :fractal-add) 0)
  (midi/midi-control sink (cc* :fractal-hue) 0)
  (midi/midi-control sink (cc* :kaleid-mod-amp) 100)
  (midi/midi-control sink (cc* :preout-mod) 10)
  (midi/midi-control sink (cc* :in-blend) 100)
  (midi/midi-control sink (cc* :voronoi-mod) 90)
  (gp/ref-rain
   :id :test
   :durs [1 2]
   :on-event (gp/on-event

              (midi/midi-control
               sink
               (cc* :hept-x-pos)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-y-pos)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-x-speed)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-y-speed)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-scale)
               (rand-int 127))
              (midi/midi-control
               sink
               (cc* :hept-repeat)
               (rand-int 127))))

  (gp/stop)
  ;; hydra
  "
await loadScript(
	'https://cdn.jsdelivr.net/npm/hydra-midi@latest/dist/index.js'
)

// Use midi messages from all channels of all inputs.
await midi.start({
	channel: '*',
	input: '*'
})
// Show a small midi monitor (similar to hydra's `a.show()`).
midi.show()

//s0.initScreen()


// cc
blackFade = 74
fractalAdd = 75
fractalHue = 76
kaleidModulateAmp = 77
preOutMod = 78
inBlend = 79
voronoiMod = 80
heptXSpeed = 81
heptXPos = 82
heptYSpeed = 83
heptYPos = 84
heptScale = 85
heptRepeat = 86
//base
noise()
	.kaleid(10)
	.mult(osc(5, 0.1, 3)
		.kaleid())
	.modulateRotate(noise(), 0.2)
	.out(o1)

//shapes
voronoi(2, 3, 10)
	.mult(osc(1, 2, 0.9), 1.5)
	.modulate(shape(7, 0.8)
		.scale(() => cc(heptScale)() * 0.5)
		.repeat(() => cc(heptRepeat)() * 20)
		.mult(osc(1, 1.5, 0.9)
			.kaleid(), 4)
		.scrollX(cc(heptXPos), () => cc(heptXSpeed)() * 2 - 1)
		.scrollY(cc(heptYPos), () => cc(heptYSpeed)() * 2 - 1)
		.rotate(0, 0.1)
		.modulateRotate(voronoi()), 6)
	// 	.add(shape(7)
	// 		.scale(0.5)
	// 		.mult(osc(1, 0.5, 0.9)
	// 			.kaleid())
	// 		.scrollX([0, 0.5, 0.3], 0.1)
	// 		.scrollY(0, 0.1)
	// 		.modulateRotate(noise(1)), 0.1)
	// 	.add(shape(7)
	// 		.repeat([3, 1, 5, 10])
	// 		.mult(osc(1, 0.5, 0.9)
	// 			.kaleid())
	// 		.scrollX(0, 0.03)
	// 		.scrollY(0, -0.1), 0.1)
	.blend(o3, 0.97)
	.out(o3)

src(o1)

	.add(src(o2)
		.scale(() => Math.sin(time * 0.7) * 0.1 + 1, () => Math.sin(time * 0.8) * 0.1 + 1), 0.1)
	.blend(src(o2), cc(inBlend))
	.modulate(voronoi()
		.saturate(1.2), () => cc(voronoiMod)() * 4)
	.add(src(o2)
		.saturate(5)
		.modulateRotate(noise()), 0.01)
	//.modulate(src(o2), 0.2)

	.hue(cc(fractalHue))
	//.saturate(1.5)
	//.brightness(-0.04)
	.add(src(o2)
		.scale(1.2), () => cc(fractalAdd)() * 1.5 * 0.95)
	.mult(solid(.7, 0.4, 0.4), () => cc(fractalAdd)() * 1.5 * 0.8)
	.blend(src(o2)
		.modulateScale(noise(0.1), 0.8)
		.kaleid(7), cc(kaleidModulateAmp))
	.diff(o3, 1)

	.modulate(src(o2), cc(preOutMod))
	//.mult(solid(1, 0.5, 1))
	//.mult(solid(), cc(blackFade))
	.out(o2)

src(o2)
	.add(src(s0)
		.modulate(o0, 0.01)
		.scale(0.9 * 16 / 9, 0.9), 0.3)

	.out()
")
