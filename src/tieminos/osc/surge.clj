(ns tieminos.osc.surge
  (:require
   [clojure.string :as str]
   [erv.scale.scl :as scl]
   [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn init []
  (when-not @osc-client
    (reset! osc-client (osc/osc-client "0.0.0.0" 53280))
    @osc-client))


(defn set-tuning
  ;; NOTE avoid extension
  [path-to-scl]
  (osc/osc-send @osc-client
                "/tuning/scl"
                path-to-scl))

(defn set-scale
  [{:keys [scale scale-name path]
    :or {path "/Users/diego/Music/tunings/"}}]
  (let [file-path (format "%s/%s.scl" path scale-name)]
    (scl/spit-file file-path scale)
    (set-tuning (str/replace file-path #".scl" ""))))

(comment
  (init)
  (set-tuning "7-11")
  (set-tuning "centaurean-shur-v1")
  (osc/osc-debug true))
