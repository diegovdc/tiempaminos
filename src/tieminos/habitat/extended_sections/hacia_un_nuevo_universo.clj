(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo
  (:require [overtone.core :as o]))

;; IDEAS
;; 1. resonant-reverberant filters with a bit of delay that come for another place vs the source sound
;; 2. explore occupying the whole of the space
;; 3. explorar pitch-shift
;; (
;; // use PitchShift to granulate input - USE HEADPHONES to prevent feedback.
;; // upper left corner is normal playback. x = pitch dispersion, y = time dispersion
;; var grainSize;
;; grainSize = 0.1;
;; play({
;; 	[SoundIn.ar([0, 1]),
;;     PitchShift.ar(
;;         SoundIn.ar([0, 1]),
;;         grainSize,
;;         MouseX.kr(1,3/2),                        // nominal pitch rate = 1
;;         0,             // pitch dispersion
;;         MouseY.kr(0, grainSize)    // time dispersion
;; 	)]
;; }))
;; ;;
;;
;; 4. explorar diferentes armonías
