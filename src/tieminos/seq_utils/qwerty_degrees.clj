(ns tieminos.seq-utils.qwerty-degrees)

(def key-levels
  ["zxcvbnm,./"
   "asdfghjkl;'"
   "qwertyuiop[]"
   "1234567890-="])

(def level-offsets
  "Key level -> offset (i.e. degrees down)"
  {0 0
   1 -1
   2 -2
   3 -3})

(def iso-keys->degs-map
  (memoize
   (fn  [starting-deg x y]
     (->> key-levels
          (map-indexed
           (fn [i level]
             (map-indexed
              (fn [j k]
                [k (+ starting-deg
                      (* y
                         (+ (* j x)
                            (level-offsets i))))])
              level)))
          (apply concat)
          (into {})))))
(->> (iso-keys->degs-map 20 2 1)
     #_(map first))

