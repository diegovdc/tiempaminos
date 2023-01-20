(ns tieminos.scratch.intervals
  (:require [erv.utils.conversions :as conv]))

(conv/ratio->cents (/ 11/8 19/18)) ;; 457.7149279632283

(conv/ratio->cents (/ 4/3 19/18)) ;; 404.4419847330848
