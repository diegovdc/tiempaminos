(ns tieminos.compositions.garden-earth.moments.two.ajax
(:require
 [tieminos.compositions.garden-earth.web.ajax :refer [post]]))

(defn post-live-state
  [data]
  (post "/in-volcanic-times/live-state" data))


(comment
  (post-live-state {:hola :mundo})
  )
