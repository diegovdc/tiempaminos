(ns tieminos.midi.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [overtone.sc.node :refer [node?]]
   [tieminos.midi.core :as midi :refer [add-synth auto-gate-fns
                                        get-note-synths remove-synth round-robin-state
                                        synths]]))

(deftest add-synth-test
  (testing "Can add a single synth to a note"
    (is (= {5 [{:i-am :synth}]}
           (with-redefs [synths (atom {})
                         node? map?]
             (add-synth {:note 5} {:i-am :synth})))))

  (testing  "Can add multiple synths for a single note"
    (is (= {5 [[{:i-am :synth} {:i-am :synth2}]]}
           (with-redefs [synths (atom {})
                         node? map?]
             (add-synth {:note 5} [{:i-am :synth}
                                   {:i-am :synth2}])))))

  (testing "Can add multiple synths for multiple repeated notes"
    (is (= {5 [{:i-am :synth} {:i-am :synth2}]}
           (with-redefs [synths (atom {})
                         node? map?]
             (add-synth {:note 5} {:i-am :synth})
             (add-synth {:note 5} {:i-am :synth2})))))

  (testing "Can add multiple synths for multiple different notes"
    (is (= {5 [{:i-am :synth}]
            6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {})
                         node? map?]
             (add-synth {:note 5} {:i-am :synth})
             (add-synth {:note 6} {:i-am :synth2}))))))

(deftest remove-synth-test
  (testing "Can remove a single synth from a note"
    (is (= {6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {5 [{:i-am :synth}]
                                       6 [{:i-am :synth2}]})
                         node? map?]
             (remove-synth assoc {:note 5})
             @synths))))
  (testing "Can remove a single multi-synth from a note"
    (is (= {6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {5 [[{:i-am :synth} {:i-am :synth2}]]
                                       6 [{:i-am :synth2}]})
                         node? map?]
             (remove-synth assoc {:note 5})
             @synths))))

  (testing "Can remove a single synth from a note but if note has many synths will keep the others"
    (is (= {5 [{:i-am :synth3}]
            6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {5 [{:i-am :synth} {:i-am :synth3}]
                                       6 [{:i-am :synth2}]})
                         node? map?]
             (remove-synth assoc {:note 5})
             @synths))))
  (testing "Can remove a single multi-synth from a note but if note has many synths will keep the others"
    (is (= {5 [[{:i-am :synth4} {:i-am :synth5}]]
            6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {5 [[{:i-am :synth} {:i-am :synth3}]
                                          [{:i-am :synth4} {:i-am :synth5}]]
                                       6 [{:i-am :synth2}]})
                         node? map?]
             (remove-synth assoc {:note 5})
             @synths))))
  (testing "Can remove several synths, one after another, associated to the same note"
    (is (= {6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {5 [{:i-am :synth} {:i-am :synth3}]
                                       6 [{:i-am :synth2}]})
                         node? map?]
             (remove-synth assoc {:note 5})
             (remove-synth assoc {:note 5})
             @synths)))
    (is (= {6 [{:i-am :synth2}]}
           (with-redefs [synths (atom {5 [{:i-am :synth} [{:i-am :synth3} {:i-am :synth4}]]
                                       6 [{:i-am :synth2}]})
                         node? map?]
             (remove-synth assoc {:note 5})
             (remove-synth assoc {:note 5})
             @synths)))))

(deftest handle-midi-event-test
  (let [handler-config {:auto-ctl? true
                        :note-on (fn [ev] {:i-am-synth (:id ev)})
                        :note-off (fn [_ev] nil)}]
    (testing "Can add a new synth"
      (is (=  {5 [{:i-am-synth 1}]}
              (with-redefs [synths (atom {})
                            node? map?]
                (#'midi/handle-midi-event {:command :note-on :note 5 :id 1}
                                          handler-config)
                @synths))))

    (testing "Can kill a synth"
      (is (= {:synths {6 [{:i-am-synth 2}]}
              :killed-synths [{:i-am-synth 1, :gate 0}]}
             (let [synths (atom {5 [{:i-am-synth 1}]
                                 6 [{:i-am-synth 2}]})
                   killed-synths (atom [])
                   ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))]
               (with-redefs [synths synths
                             node? map?
                             auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                 (#'midi/handle-midi-event {:command :note-off :note 5}
                                           handler-config)
                 {:synths @synths
                  :killed-synths @killed-synths})))))

    (testing "Can add a synth on the same note"
      (is (= {:synths {5 [{:i-am-synth 1} {:i-am-synth 3}]
                       6 [{:i-am-synth 2}]}
              :killed-synths []}
             (let [synths (atom {5 [{:i-am-synth 1}]
                                 6 [{:i-am-synth 2}]})
                   killed-synths (atom [])]
               (with-redefs [synths synths
                             node? map?]
                 (#'midi/handle-midi-event {:command :note-on :note 5 :id 3}
                                           handler-config)
                 {:synths @synths
                  :killed-synths @killed-synths}))))
      (is (= {:synths {5 [{:i-am-synth 1} {:i-am-synth 3} {:i-am-synth 4} {:i-am-synth 5}]
                       6 [{:i-am-synth 2}]}
              :killed-synths []}
             (let [synths (atom {5 [{:i-am-synth 1}]
                                 6 [{:i-am-synth 2}]})
                   killed-synths (atom [])]
               (with-redefs [synths synths
                             node? map?]
                 (#'midi/handle-midi-event {:command :note-on :note 5 :id 3}
                                           handler-config)
                 (#'midi/handle-midi-event {:command :note-on :note 5 :id 4}
                                           handler-config)
                 (#'midi/handle-midi-event {:command :note-on :note 5 :id 5}
                                           handler-config)
                 {:synths @synths
                  :killed-synths @killed-synths})))))

    (testing "Can add and kill a synth on the same note"
      (is (= {:synths {5 [{:i-am-synth 3}]
                       6 [{:i-am-synth 2}]}
              :killed-synths [{:i-am-synth 1, :gate 0}]}
             (let [synths (atom {5 [{:i-am-synth 1}]
                                 6 [{:i-am-synth 2}]})
                   killed-synths (atom [])
                   ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))]
               (with-redefs [synths synths
                             node? map?
                             auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                 (#'midi/handle-midi-event {:command :note-on :note 5 :id 3}
                                           handler-config)
                 (#'midi/handle-midi-event {:command :note-off :note 5}
                                           handler-config)
                 {:synths @synths
                  :killed-synths @killed-synths})))))

    (testing "Can add and kill a synths in alternation"
      (is (= {:synths {7 [{:i-am-synth 3}]
                       8 [{:i-am-synth 4}]}
              :killed-synths [{:i-am-synth 1, :gate 0}
                              {:i-am-synth 2, :gate 0}]}
             (let [synths (atom {5 [{:i-am-synth 1}]
                                 6 [{:i-am-synth 2}]})
                   killed-synths (atom [])
                   ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))]
               (with-redefs [synths synths
                             node? map?
                             auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                 (#'midi/handle-midi-event {:command :note-on :note 7 :id 3}
                                           handler-config)
                 (#'midi/handle-midi-event {:command :note-off :note 5}
                                           handler-config)
                 (#'midi/handle-midi-event {:command :note-off :note 6}
                                           handler-config)
                 (#'midi/handle-midi-event {:command :note-on :note 8 :id 4}
                                           handler-config)
                 {:synths @synths
                  :killed-synths @killed-synths})))))

    (testing "dup-note-mode: round-robin"
      (testing "Can add and kill a synths on a single note in round-robin fashion"
        (is (= {:synths {5 [{:i-am-synth 4}]
                         6 [{:i-am-synth 2}]}
                :killed-synths [{:i-am-synth 1, :gate 0}
                                {:i-am-synth 3, :gate 0}]
                :round-robin-state {:held-keys {5 2}}}
               (let [synths (atom {5 [{:i-am-synth 1}]
                                   6 [{:i-am-synth 2}]})
                     killed-synths (atom [])
                     ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))
                     handler-config* (assoc handler-config :dup-note-mode :round-robin)]
                 (with-redefs [synths synths
                               round-robin-state (atom {})
                               node? map?
                               get-note-synths  (fn [note] (get @synths note))
                               auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 3} handler-config*)
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 4} handler-config*)
                   {:synths @synths
                    :killed-synths @killed-synths
                    :round-robin-state @round-robin-state})))))

      (testing "will not call note-off every time a synth is killed, only when there is an actual note-off. Note that the round-robin-state tracks the notes that are still held down, regardless of the amount of synths that are being played"
        (is (= {:synths {5 [{:i-am-synth 4}]}
                :killed-synths [{:i-am-synth 3, :gate 0}]
                :round-robin-state {:held-keys {5 2}}}
               (let [synths (atom {})
                     killed-synths (atom [])
                     ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))
                     handler-config* (assoc handler-config :dup-note-mode :round-robin)]
                 (with-redefs [synths synths
                               node? map?
                               round-robin-state (atom {})
                               get-note-synths  (fn [note] (get @synths note))
                               auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 3} handler-config*)
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 4} handler-config*)
                   {:synths @synths
                    :killed-synths @killed-synths
                    :round-robin-state @round-robin-state}))))

        (is (= {:synths {5 [{:i-am-synth 4}]}
                :killed-synths [{:i-am-synth 3, :gate 0}]
                :round-robin-state {:held-keys {5 1}}}
               (let [synths (atom {})
                     killed-synths (atom [])
                     ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))
                     handler-config* (assoc handler-config :dup-note-mode :round-robin)]
                 (with-redefs [synths synths
                               node? map?
                               round-robin-state (atom {})
                               get-note-synths  (fn [note] (get @synths note))
                               auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 3} handler-config*)
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 4} handler-config*)
                   (#'midi/handle-midi-event {:command :note-off :note 5} handler-config*)
                   {:synths @synths
                    :killed-synths @killed-synths
                    :round-robin-state @round-robin-state}))))

        (is (= {:synths {5 [{:i-am-synth 5}]}
                :killed-synths [{:i-am-synth 3, :gate 0}
                                {:i-am-synth 4, :gate 0}]
                :round-robin-state {:held-keys {5 2}}}
               (let [synths (atom {})
                     killed-synths (atom [])
                     ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))
                     handler-config* (assoc handler-config :dup-note-mode :round-robin)]
                 (with-redefs [synths synths
                               node? map?
                               round-robin-state (atom {})
                               get-note-synths  (fn [note] (get @synths note))
                               auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 3} handler-config*)
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 4} handler-config*)
                   (#'midi/handle-midi-event {:command :note-off :note 5} handler-config*)
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 5} handler-config*)
                   {:synths @synths
                    :killed-synths @killed-synths
                    :round-robin-state @round-robin-state})))))

      (testing "round-robin will kill all synths if previously there were several on a single note sounding at once"
        (is (= {:synths {5 [{:i-am-synth 2}]}
                :killed-synths [{:i-am-synth 1, :gate 0}]
                :round-robin-state {:held-keys {5 1}}
                :note-off-calls [{:command :note-off, :note 5, :note-off-call? true}]}
               (let [synths (atom {})
                     killed-synths (atom [])
                     note-off-calls (atom [])
                     ctl (fn [synth-map gate v] (swap! killed-synths conj (assoc synth-map gate v)))
                     handler-config* (assoc handler-config
                                            :dup-note-mode :round-robin
                                            :note-off (fn [ev] (swap! note-off-calls conj ev)))]
                 (with-redefs [synths synths
                               node? map?
                               round-robin-state (atom {})
                               auto-gate-fns {:add add-synth :remove (partial remove-synth ctl)}]
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 1} handler-config*)
                   (#'midi/handle-midi-event {:command :note-on :note 5 :id 2} handler-config*)
                   (#'midi/handle-midi-event {:command :note-off :note 5 :note-off-call? true} handler-config*)
                   {:synths @synths
                    :killed-synths @killed-synths
                    :round-robin-state @round-robin-state
                    :note-off-calls @note-off-calls}))))))))
