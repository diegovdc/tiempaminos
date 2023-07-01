(ns tieminos.lumatone.general-keyboard
  "See 20230604174831-lumatone_coordinate_system.org"
  (:require
   [clojure.string :as str]))

;; TODO Map keys per board into a coordinate system
;;;; DONE All starting keys (in the x axis should be mapped)
;;;;;; DONE Use ranges to fill in the whole mapping
;;;;;;;; DONE Convert all boards into a single coord-map->board-key...
;;;;;;;;;; TODO Generate board from there
;; TODO use coordinate system to create lumatone mapping files (distribute notes, midi data and colors)


(def board-base-coords
  "Basic coordinates (based on the first board) and x-axis length, using right most keys in the `x` axis, for every distinct `y`"
  ;; Left: (-4, 2), (-3, 3), (-3, 4), (-2, 5), (-2, 6), (-1, 7), (-1, 8), (0, 9), (0, 10)
  {0 {:coord [0 10] :x-len 2}
   2 {:coord [0 9] :x-len 5}
   7 {:coord [-1 8] :x-len 6}
   13 {:coord [-1 7] :x-len 6}
   19 {:coord [-2 6] :x-len 6}
   25 {:coord [-2 5] :x-len 6}
   31 {:coord [-3 4] :x-len 6}
   37 {:coord [-3 3] :x-len 6}
   43 {:coord [-4 2] :x-len 6}
   49 {:coord [-3 1] :x-len 5}
   54 {:coord [-1 0] :x-len 2}})

(def board-coord-offsets
  "All +5x -1y"
  {0 [0 0]
   1 [5 -2]
   2 [10 -4]
   3 [15 -6]
   4 [20 -8]})

(defn board-keys
  "Make a map from board key to coord"
  [board-n]
  (let [[x-offset y-offset] (board-coord-offsets board-n)]
    (->> board-base-coords
         (map
          (fn [[k-num {:keys [x-len]
                       [x y] :coord}]]
            (->> (range x-len)
                 (map (fn [local-x-offset]
                        [(+ k-num local-x-offset)
                         [(+ x local-x-offset x-offset)
                          (+ y y-offset)]]))
                 (into {}))))
         (apply merge))))

(def board-fields
  [[:key "Key_%s"]
   [:chan "Chan_%s"]
   [:color "Col_%s"]
   [:k-type "KTyp_%s"]
   [:cc-invert "CCInvert_%s"]])

(defn coord->board-key*
  "Create a map of coords->board-key data for a single board"
  [board-n]
  (->> (board-keys board-n)
       (map (fn [[k coord]]
              [coord (into {:board/id board-n
                            :board/key k}
                           (map (fn [[field-k field-name]]
                                  [field-k (format field-name k)])
                                board-fields))]))
       (into {})))

(def coord->board-key
  (->> (range 5)
       (map coord->board-key*)
       (apply merge)))

(comment
  (->  coord->board-key))

(do (defn wcoord->lcoord
      "Convert wilson's scale tree coords to lumatone coords. The systems are rotated "
      [[x y]]
      [y (* -1 x)])
    (wcoord->lcoord [7 5]))

(defn add-coords
  [coord-1 coord-2]
  (map + coord-1 coord-2))

(do
  (defn coord-interval
    [reference-coord target-coord]
    (map - target-coord reference-coord))
  (coord-interval [0 0] [3 -1]))

(defn get-board-xy-intervals
  "Get the interval size of `x` and `y` with respect to the linear scale.

  It's just a brute force way to solve a simple system of equations."
  [gen gen-coords period period-coords]
  (let [[gx gy] gen-coords
        [px py] period-coords
        range* (range (dec (* -1 period)) (inc period))]
    (->> (for [x range*, y range*]
           (and
            (= gen (+ (* gx x) (* gy y)))
            (= period (+ (* px x) (* py y)))
            {:x x :y y}))
         (filter identity)
         first)))

(get-board-xy-intervals 7 [3 -1] 12 [5 -2])

(defn coord->scale-degree
  [xy-intervals coord]
  (let [[x y] coord]
    (+ (* x (:x xy-intervals))
       (* y (:y xy-intervals)))))

(do
  (defn midi-chan-tranpose
    "Pianoteq style midi channel transposition"
    [notes-per-period midi-note]
    ;; If note-diff > 0 then we have exceeded the amount of notes ina midi-channel
    (let [note-diff (- midi-note 128)]
      (if (<= note-diff 0)
        {:key midi-note
         :chan 1}
        {:key (+ note-diff (- 128 notes-per-period))
         :chan (+ 2 (quot note-diff notes-per-period))})))
  (midi-chan-tranpose 31 129))

(comment
  ;; test calculation for basic keyboard data
  (let [offset 28
        n-tones 31
        xy-intervals {:x 5 :y -3} #_{:x 2 :y -1}]
    (->> coord->board-key
         (sort-by (juxt (comp :board/id second)
                        (comp :board/key second)))
         (map (fn [[coord {:keys [board/id key chan color k-type cc-invert]}]]
                (let [midi-data (midi-chan-tranpose n-tones
                                                    (+ offset (coord->scale-degree xy-intervals
                                                                                   coord)))]
                  (cond->
                   {key (:key midi-data)
                    chan (:chan midi-data)}
                    true (merge {:board/id id
                                 color "000000"
                                 k-type 0
                                 cc-invert true}))))))))

(do
  (defn diophantine-fractions
    ;; cf. page 12 of https://www.anaphoria.com/gralkeyboard.pdf
    "c/d = a/b + e/f
  gen = c
  period = d
  a + e = c
  b + f = d"
    [gen period]
    (let [c gen
          d period]
      (->> (for [a (range 1 (inc d))
                 b (range 1 (inc d))
                 e (range 1 (inc d))
                 f (range 1 (inc d))]
             (and (= c (+ a e))
                  (= d (+ b f))
                  (= 1 (- (* b e) (* a f)))
                  (= 1 (- (* b c) (* a d)))
                  (= 1 (- (* d e) (* c f)))
                  {:a a :b b :c c :d d :e e :f f}))
           (filter identity)
           first)))
  (diophantine-fractions 11 19)
  (diophantine-fractions 7 12)
  (diophantine-fractions 4 7)
  (diophantine-fractions 3 5)
  (diophantine-fractions 2 3)
  (diophantine-fractions 18 31))

(do
  (defn generate-keyboard-types*
    "Finds a path to the root of the scale tree from the fraction
  made of `gen/period`.
  Returns a map from keyboard-type to diophantine and xy-interval data."
    [gen period]
    (->> (loop [fractions [[[gen period] {}]]]
           (let [[gen period] (first (last fractions))
                 {:keys [a b e f] :as diophantine-data}  (diophantine-fractions gen period)]
             (if (and a b e f)
               (recur (conj fractions
                            ;; NOTE not sure about this
                            [(if (> b f)
                               [a b]
                               [e f])
                             diophantine-data]))
               fractions)))
         ;; remove the first fraction which has and empty map as data
         (drop 1)
         ;;
         (map (fn [[_ {:keys [a b c d e f] :as diophantine-data}]]
                [[c d] (assoc diophantine-data
                              :xy-intervals
                              (get-board-xy-intervals
                               gen (wcoord->lcoord [a e])
                               period (wcoord->lcoord [b f])))]))
         (into {})))

  (generate-keyboard-types* 7 12)
  #_(generate-keyboard-types 18 31))
(do
  (defn generate-keyboard-types->xy-intervals
    "Finds a path to the root of the scale tree from the fraction
  made of `gen/period`.
  Returns a map from keyboard-type to xy-interval data."
    [gen period]
    (->> (generate-keyboard-types* gen period)
         (map (fn [[k d]]
                [k (:xy-intervals d)]))
         (into {})))

  (generate-keyboard-types->xy-intervals 7 12)
  (generate-keyboard-types->xy-intervals 18 31))

(comment
  ;; WIP make keyboard
  (let [offset 28
        gen 18
        period 31
        xy-intervals ((generate-keyboard-types->xy-intervals 18 31)
                      [4 7])]
    (->> coord->board-key
         (sort-by (juxt (comp :board/id second)
                        (comp :board/key second)))
         (map (fn [[coord {:keys [key chan]}]]
                (let [midi-data (midi-chan-tranpose period
                                                    (+ offset (coord->scale-degree xy-intervals
                                                                                   coord)))]
                  {key (:key midi-data)
                   chan (:chan midi-data)}))))))

(do
  (defn make-ltn-data
    [{:keys [offset period xy-intervals color-fn]
      :or {color-fn (fn [_midi-note] "111111")}}]
    (->> coord->board-key
         (sort-by (juxt (comp :board/id second)
                        (comp :board/key second)))
         (map (fn [[coord key-data]]
                (let [midi-data (midi-chan-tranpose period
                                                    (+ offset (coord->scale-degree xy-intervals
                                                                                   coord)))]
                  (merge {:key-value (:key midi-data)
                          :chan-value (:chan midi-data)
                          :color-value (color-fn (:key midi-data))}
                         key-data))))))
  (map (juxt :chan-value :key :key-value)
       (let [gen 18 period 31]
         (make-ltn-data
          {:offset 28
           :period period
           :xy-intervals ((generate-keyboard-types->xy-intervals gen period)
                          [4 7])}))))

(defn format-val
  ([k] k)
  ([k v] (format "%s=%s" k v)))
(do
  (defn ltn-data->ltn
    [ltn-data]
    (->> ltn-data
         (sort-by :board/key)
         (group-by :board/id)
         (mapcat (fn [[board-id keys-data]]
                   (concat
                    [(format "[Board%s]" board-id)]
                    (mapcat (fn [{:keys [key key-value chan chan-value color cc-invert k-type]}]
                              (let [key-exists? (> key-value -1)]
                                (->> [(format-val key (if key-exists?
                                                        key-value 0))
                                      (format-val chan chan-value)
                                      (format-val color "000000")
                                      (when-not key-exists?
                                        (format-val k-type 0))
                                      cc-invert]
                                     (remove nil?))))
                            keys-data))))
         (str/join "\n")))

  (spit "/Users/diego/Music/diego/lumatone/31_gen-18_4-7_template.ltn"
        (ltn-data->ltn
         (let [gen 18 period 31]
           (make-ltn-data
            {:offset 28
             :period period
             :xy-intervals ((generate-keyboard-types->xy-intervals gen period)
                            [4 7])})))))
