(ns tieminos.compositions.7D-percusion-ensamble.dreams.hydra-control
  (:require
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.utils :refer [cb-interpolate]]))

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
        (catch Exception _ (timbre/error "Unknown path" msg)))
      ))


  (def cc {
           :black-fade 74
           :fractal-add 75
           :fractal-hue 76
           :kaleid-mod-amp 77
           :preout-mod 78
           :in-blend 79
           :voronoi-mod 80
           }
    )

  (midi/midi-control sink (cc :black-fade) 0)
  (midi/midi-control sink (cc :fractal-add) 0)
  (midi/midi-control sink (cc :fractal-hue) 0)
  (midi/midi-control sink (cc :kaleid-mod-amp) 0)
  (midi/midi-control sink (cc :preout-mod) 0)
  (midi/midi-control sink (cc :in-blend) 0)
  (midi/midi-control sink (cc :voronoi-mod) 0))
