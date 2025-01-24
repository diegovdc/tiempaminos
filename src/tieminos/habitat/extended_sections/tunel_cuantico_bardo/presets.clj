(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.presets
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.osc.reaper :as reaper]))

(def presets-dir
  (str (System/getProperty "user.dir")
       "/src/tieminos/habitat/extended_sections/tunel_cuantico_bardo/presets/"))

(defn save-preset!
  []
  (let [date (java.util.Date.)
        file-name (-> (random-uuid)
                      str
                      (str/split #"-")
                      first
                      (str ".edn"))]
    (timbre/info "saving preset:" file-name)
    (spit
     (str presets-dir file-name)
     (str {:date date
           :touch-osc-state @bardo.live-state/touch-osc-state
           :live-state (dissoc @bardo.live-state/live-state :lorentz)}))
    (reaper/basic-insert-marker file-name)))

(def loadable-touch-osc-params
  {"/Milo/rec-durs-radio" int
   "/Milo/rec-pulse-radio" int
   "/Milo/clouds-amp" float
   "/Milo/clouds-env-radio" int
   "/Milo/clouds-rhythm-radio"  int
   "/Milo/clouds-sample-lib-size-radio" int
   "/Milo/synth-radio" int
   "/Milo/harmony-radio" int
   "/Milo/harmonic-speed" float
   "/Milo/harmonic-lowest-note" float
   "/Milo/harmonic-highest-note" float
   "/Milo/rev-send-clean" float
   "/Milo/rev-send-process" float
   "/Diego/rec-durs-radio" int
   "/Diego/rec-pulse-radio" int
   "/Diego/clouds-amp" float
   "/Diego/clouds-env-radio" int
   "/Diego/clouds-rhythm-radio" int
   "/Diego/clouds-sample-lib-size-radio" int
   "/Diego/synth-radio" int
   "/Diego/harmony-radio" int
   "/Diego/harmonic-speed" float
   "/Diego/harmonic-lowest-note" float
   "/Diego/harmonic-highest-note" float
   "/Diego/rev-send-clean" float
   "/Diego/rev-send-process" float
   "/gusano/gusano-active-milo-src-btn" float
   "/gusano/gusano-active-diego-src-btn" float
   "/gusano/rates" int
   "/gusano/rates-seq-speed" float
   "/gusano/amp" float
   "/gusano/period" int
   "/gusano/durs" int
   "/gusano/grain-trig" float
   "/gusano/grain-durs" float
   "/gusano/2nd-voice" int})

(defn load-preset!
  [internal-client clients file-name]
  (try
    (let [{:keys [touch-osc-state]} (edn/read-string (slurp (str presets-dir file-name)))]
      (doseq [[path args] (select-keys touch-osc-state  (keys loadable-touch-osc-params))]
        ;; Send osc data to internal receiver so that all data can be updated
        ;; TODO selected banks are missing, but may be useful
        (osc/osc-send internal-client
                      path
                      (let [format-fn (get loadable-touch-osc-params path)
                            arg (first args)]
                        (format-fn arg))))
      (doseq [[_ client] clients]
        (osc/osc-send client "/presets/last-selected-preset-name" file-name)))
    (catch Exception e
      (timbre/error (format "Could not load preset for: %s" file-name)
                    e))))
