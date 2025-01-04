(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.osc.reaper :as reaper]))

(defonce touch-osc-state (atom {}))
(defonce live-state (atom {:lorentz (lorentz/init-system :x 0.3 :y 0.02 :z 0.012)}))

(comment

  (-> (random-uuid)
      str
      (str/split #"-")
      first)

  (str {:date (java.util.Date.)
        :touch-osc-state @touch-osc-state
        :live-state (dissoc @live-state :lorentz)}))

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
           :touch-osc-state @touch-osc-state
           :live-state (dissoc @live-state :lorentz)}))
    (reaper/basic-insert-marker file-name)))

(defn load-preset!
  [file-name]
  ;; TODO finish implementing
  ;; Perhaps get the touch-osc-state values that are of interest
  ;; and the send them one by one to the osc handler
  ;; this will load them to touch-osc and to the live state
  (edn/read-string (slurp (str presets-dir file-name))))

(comment
  (save-preset!)

  (load-preset! "9082b4bc.edn")

  (edn/read-string
   (slurp
    (str (System/getProperty "user.dir")
         "/src/tieminos/habitat/extended_sections/tunel_cuantico_bardo/test.edn"))))

(defn init-watch!
  [id f]
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (f new-value))))

(defn get-active-banks
  [player-k]
  (->> @live-state :algo-2.2.9-clouds player-k :active-banks))

(comment
  (->> @live-state))
