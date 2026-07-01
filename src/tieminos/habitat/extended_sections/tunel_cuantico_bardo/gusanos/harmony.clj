(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.harmony
  (:require
   [tieminos.habitat.extended-sections.harmonies.chords :refer [get-harmony
                                                                rate-chord-seq
                                                                transpose-chord]]))

(comment
  (count (get-harmony :meta-pelog-20))

  (map :ratio (get-harmony :meta-pelog-20)))

(defn make-rates
  [harmony-k]
  (let [harmony (get-harmony harmony-k)
        chord-fn  (partial rate-chord-seq harmony)]
    [(let [chord [0 5 13 21]
           chords (chord-fn (transpose-chord chord (range 0 (* 21 6) 5)))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " range: 0,21*6 | step: 5")}))
     (let [chord [0 5 13 21]
           chords (chord-fn (transpose-chord chord (range (* 21 -3) (* 21 6) 5)))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " range: -21*3,21*6 | step: 5")}))
     (let [chord [20 19 2 27 23 34 50 48]
           chords (interleave
                   (chord-fn (transpose-chord [0 9 16] chord))
                   (chord-fn (transpose-chord [8] chord))
                   (chord-fn (transpose-chord [-2 13 18] chord))
                   (chord-fn (transpose-chord [3] chord))
                   (chord-fn (transpose-chord [-15 21] chord))
                   (chord-fn (transpose-chord [13] chord)))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " complex transp.")}))
     ;; fib: acorde bonito, muy liso
     (let [chord [0 6 12 18]
           size (count harmony)
           chords (chord-fn (transpose-chord chord (range size)))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " range: 0," size)}))
     ;; fib: estable claro (segmento de arriba: 4-4)
     (let [chord [11 15 19]
           size (count harmony)
           chords (chord-fn (transpose-chord chord (range size)))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " range: 0," size)}))
     ;;  fib: nocturno (5-5)
     (let [chord [10 15 20]
           size (count harmony)
           chords (chord-fn (transpose-chord chord (range size)))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " range: 0," size)}))
     ;; fib: calido con un poco de disonancia
     (let [chord [0 4 8 12 16 20 24 28]
           chords (chord-fn (transpose-chord chord [0 1]))]
       (with-meta (interleave chords (reverse chords))
         {:name (str chord " range: 0,1")}))]))

(def gusano-harmonic-seqs
  {:fib (make-rates :fib)
   :meta-slendro-22 (make-rates :meta-slendro-22)
   :meta-pelog-20 (make-rates :meta-pelog-20)})

(def gusano-harmonies (keys gusano-harmonic-seqs))


