(ns tieminos.compositions.7D-percusion-ensamble.dreams.hydra-control
  (:require
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.utils :refer [cb-interpolate]]
   [time-time.dynacan.players.gen-poly :as gp]))

;; Intended to run on linux machineso the following may need to be done
;; Setup of VirMIDI: https://github.com/anton-k/linux-audio-howto/blob/master/doc/os-setup/virtual-midi.md#virtual-midi-1
;;   NOTE: it's ok to use qjack, it seems.
;; Testing: https://hydra.ojack.xyz/?code=JTJGJTJGJTIwWW91JTIwY2FuJTIwZWl0aGVyJTIwdXNlJTIwJTYwJTQwbGF0ZXN0JTYwJTIwb3IlMjBsb2FkJTIwYSUyMHNwZWNpZmljJTIwdmVyc2lvbiUyMHdpdGglMkMlMjBmb3IlMjBleGFtcGxlJTJDJTIwJTYwJTQwMC40LjAlNjAuJTBBYXdhaXQlMjBsb2FkU2NyaXB0KCUwQSUyMCUyMCdodHRwcyUzQSUyRiUyRmNkbi5qc2RlbGl2ci5uZXQlMkZucG0lMkZoeWRyYS1taWRpJTQwbGF0ZXN0JTJGZGlzdCUyRmluZGV4LmpzJyUwQSklMEElMEElMkYlMkYlMjBVc2UlMjBtaWRpJTIwbWVzc2FnZXMlMjBmcm9tJTIwYWxsJTIwY2hhbm5lbHMlMjBvZiUyMGFsbCUyMGlucHV0cy4lMEFhd2FpdCUyMG1pZGkuc3RhcnQoJTdCJTIwY2hhbm5lbCUzQSUyMCcqJyUyQyUyMGlucHV0JTNBJTIwJyonJTIwJTdEKSUwQSUyRiUyRiUyMFNob3clMjBhJTIwc21hbGwlMjBtaWRpJTIwbW9uaXRvciUyMChzaW1pbGFyJTIwdG8lMjBoeWRyYSdzJTIwJTYwYS5zaG93KCklNjApLiUwQW1pZGkuc2hvdygpJTBBJTBBJTJGJTJGJTIwVXNlJTIwYW55JTIwbm90ZSUyMHRvJTIwY29udHJvbCUyMHRoZSUyMHJlZCUyMGFtb3VudCUyMG9mJTIwaHlkcmEncyUyMCU2MHNvbGlkKCklNjAlMjBmdW5jdGlvbi4lMEElMkYlMkZzb2xpZChub3RlKCcqJyklMkMlMjAwJTJDJTIwMSkub3V0KCklMEElMEElMkYlMkYlMjBPciUyQyUyMGlmJTIweW91JTIwYXJlJTIwdXNpbmclMjBhJTIwbWlkaSUyMGNvbnRyb2xsZXIlMjBhbmQlMjBub3QlMjBhJTIwa2V5Ym9hcmQlM0ElMEElMkYlMkYlMjBVc2UlMjBhJTIwY29udHJvbCUyMGNoYW5nZSUyMHZhbHVlJTIwdG8lMjBjb250cm9sJTIwdGhlJTIwcmVkJTIwYW1vdW50LiUwQSUyMHNvbGlkKGNjKDc0KSUyQyUyMDAlMkMlMjAxKS5vdXQoKQ%3D%3D

(def sink (midi/midi-out "VirMIDI"))

(defn fade [{:keys [cc fade-to]}]
  (cb-interpolate {:id (keyword "hydra-control" (str "cc" cc))
                   :dur-ms 7000
                   :tick-ms 100
                   :init-val 0
                   :target-val fade-to
                   :cb (fn [{:keys [val]}] (midi/midi-control sink cc val))}))

(defn fade-from-black [] (fade {:cc 74 :fade-to 0}))
(defn fade-to-black [] (fade {:cc 74 :fade-to 127}))

(defn interpolate-ad-envelope
  [{:keys [id tick-ms atk-ms atk-val dcy-ms dcy-val cb]
    :or {tick-ms 100}}]
  (cb-interpolate
   {:id id
    :dur-ms atk-ms
    :tick-ms tick-ms
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

(comment
  (interpolate-ad-envelope {:id :ad-interpolation :tick-ms 500 :atk-ms 1000 :atk-val 1 :dcy-ms 2000 :dcy-val 0 :cb println}))

(comment
  (fade-from-black)
  (fade-to-black)
  (habitat-osc/init :port 7777)
  (habitat-osc/responder
   (fn [{:keys [path args] :as msg}]
     (println msg)
     (try
       (case path
         "/fade-from-black" (fade-from-black)
         "/fade-to-black" (fade-to-black))
       (catch Exception _ (timbre/error "Unknown path" msg)))))

  (def cc {:black-fade 74
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
           })

  (midi/midi-control sink (cc :black-fade) 0)
  (midi/midi-control sink (cc :fractal-add) 0)
  (midi/midi-control sink (cc :fractal-hue) 0)
  (midi/midi-control sink (cc :kaleid-mod-amp) 0)
  (midi/midi-control sink (cc :preout-mod) 0)
  (midi/midi-control sink (cc :in-blend) 0)
  (midi/midi-control sink (cc :voronoi-mod) 0))


(comment
  (gp/ref-rain
    :id :test
    :durs [1 2]
    :on-event (gp/on-event

                (midi/midi-control
                  sink
                  (cc :hept-x-pos)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-y-pos)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-x-speed)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-y-speed)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-scale)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-repeat)
                  (rand-int 127))
                )
    )
  (gp/stop)
  )


(comment
  ;;  very cool, explosion-like
  ;; extasis of the 7th dimension
  (midi/midi-control sink (cc :black-fade) 0)
  (midi/midi-control sink (cc :fractal-add) 0)
  (midi/midi-control sink (cc :fractal-hue) 0)
  (midi/midi-control sink (cc :kaleid-mod-amp) 100)
  (midi/midi-control sink (cc :preout-mod) 10)
  (midi/midi-control sink (cc :in-blend) 100)
  (midi/midi-control sink (cc :voronoi-mod) 90)
  (gp/ref-rain
    :id :test
    :durs [1 2]
    :on-event (gp/on-event

                (midi/midi-control
                  sink
                  (cc :hept-x-pos)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-y-pos)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-x-speed)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-y-speed)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-scale)
                  (rand-int 127))
                (midi/midi-control
                  sink
                  (cc :hept-repeat)
                  (rand-int 127))
                )
    )
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
"
  )
