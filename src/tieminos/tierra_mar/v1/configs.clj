(ns tieminos.tierra-mar.v1.configs
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]))

;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;

(def ^:private bh-bus (bh/make-aggregate 16 128))

(def ins
  "Buses coming in from blackhole. Will use the `Blackhole-16` devices as inputs."
  {:voz-1 1
   :voz-2 2
   :voz-3 10
   :fl-main 3
   :olivo/surge-pad 4                  ;; 2ch
   :olivo/plamonic-phantom-resonance 6 ;; 2ch
   :olivo/plamonic-hal-bop 8           ;; 2ch
   })

;; First 9 bh buses are reserved as inputs
(def outs
  "Buses going out to blackhole. Will use the `Blackhole-128` device for outputs."
  {:nubosidades-fl-2ch 1
   :nubosidades-fl2-2ch 3
   :nubosidades-arp-2ch 5
   :nubosidades-arp2-2ch 7

   :olivo-spiral-arp-28ch 9 ;; until 36
   :olivo-tree-top-30ch 37   ;; until 75

   :lluvia-voice-dome-25ch 67 ;; until 92 (actually 91, but last channel in REAPER is not used, but must be a pair number for REAPER)
   :lluvia-voice-shadow-2ch 92 ;; until 93

   :lluvia-fl-spirals-22ch 94 ;; until 124
   :lluvia-fl-shadow-2ch 116   ;; until 126

   ;; NOTE: The next section overlaps with the `nubosidades`and `olivo`, but they shouldn't be sounding by now. Tried using Blackhole 256, but Supercollider wouldn't start, at least not with the Aggregate device.
   :campo-magentismo-51ch 1 ;; until 61
   })

#_'([:nubosidades-fl-2ch 1]
    [:nubosidades-fl2-2ch 3]
    [:nubosidades-arp-2ch 5]
    [:nubosidades-arp2-2ch 7]

    [:olivo-spiral-arp-28ch 9]
    [:olivo-tree-top-30ch 37]

    [:lluvia-voice-dome-25ch 67]
    [:lluvia-voice-shadow-2ch 92]

    [:lluvia-fl-spirals-22ch 94]
    [:lluvia-fl-shadow-2ch 116]

    [:campo-magentismo-51ch 1])
(comment
  (->> {:nubosidades-fl-2ch 10
        :nubosidades-fl2-2ch 12
        :nubosidades-arp-2ch 14
        :nubosidades-arp2-2ch 16

        :olivo-spiral-arp-28ch 18 ;; until 45
        :olivo-tree-top-30ch 46   ;; until 75

        :lluvia-voice-dome-25ch 76 ;; until 100 (actually 101, but last channel in REAPER is not used, but must be a pair number for REAPER)
        :lluvia-voice-shadow-2ch 101 ;; until 102

        :lluvia-fl-spirals-22ch 103 ;; until 124
        :lluvia-fl-shadow-2ch 125   ;; until 126

        ;; NOTE: The next section overlaps with the `nubosidades`and `olivo`, but they shouldn't be sounding by now. Tried using Blackhole 256, but Supercollider wouldn't start, at least not with the Aggregate device.
        :campo-magentismo-51ch 10 ;; until 61
        }

       (map (juxt first (comp #(- % 9) second))))
  ;; total channels
  (+ 9
     2 2 2 2
     28 30
     25 2 22 2
     #_51))

(defn get-input [k]
  (if-let [bus (ins k)]
    (bh-bus :16 bus)
    (throw (ex-info "In bus not found" {:key k}))))

(defn get-output [k]
  (if-let [bus (outs k)]
    (bh-bus :128 bus)
    (throw (ex-info "Out bus not found" {:key k}))))

;; IO Buses
;;;;;;;;;

(defonce audio-buses (atom {}))

(defn init-buses!
  []
  (reset! audio-buses
          (reduce-kv
           (fn [m k v] (assoc m k (o/audio-bus v (name k))))
           {}
           {:arp->nubosidad 2
            :arp->nubosidad2 2})))

(defn get-audio-bus
  [k]
  (if-let [bus (get @audio-buses k)]
    bus
    (timbre/error "Audio Bus not found" {:key k})))

(comment
  (init-buses!))

;;;;;;;;;;;;;;;;;;
;; IEM
;;;;;;;;;;;;;;;;;;

(def iem-osc-ports
  {:nubosidad-lorenztiana-fl 1234
   :nubosidad-lorenztiana-fl2 1235
   :nubosidad-lorenztiana-arp 1236
   :nubosidad-lorenztiana-arp2 1237
   :olivo-copa 1238
   :olivo-spiral-arp 1239
   :lluvia-viento-voz 2345
   :lluvia-viento-fl 2346
   :lluvia-viento-voz-shadow 2347
   :lluvia-viento-fl-shadow 2348
   :campo-magnetismo 3456})

(defonce iem-osc-clients
  (atom nil))

(defn init-osc-clients!
  []
  (doseq [[k {:keys [client]}] @iem-osc-clients]
    (timbre/info "Closing to reinit:" k)
    (osc/osc-close client))
  (reset! iem-osc-clients
          (->> iem-osc-ports
               (map (fn [[k port]]
                      [k {:client (osc/osc-client "localhost" port)
                          :port port}]))
               (into {}))))

(defn get-iem-osc-client
  [k]
  (if-let [c (-> @iem-osc-clients k :client)]
    c
    (throw (ex-info "IEM OSC client not found. Maybe call `tieminos.tierra-mar.v1.configs/init-osc-clients!` first"
                    {:key k}))))

(comment
  (get-iem-osc-client :nubosidad-lorenztiana-fl)
  (init-osc-clients!))
