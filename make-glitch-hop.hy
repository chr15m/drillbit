#!/usr/bin/env hy

(import
  [sys [argv stderr]]
  [random [Random]]
  [pprint [pprint]]
  [autotracker.utils [track-builder initial-hash extract-hash add-message]]
  [autotracker.compose [get-good-notes mutate-notes]]
  [generators [make_bleep_crunch make_808er]])

(defn pr [note val] (pprint {note val} stderr))

(defn play-pattern [c p patterns]
  (get
    (-> patterns (get c) (get :pattern))  
    (% (int (/ p (-> patterns (get c) (get :pace)))) 2)))

(defn main [argv]
  ;(print "start")
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sample pattern] (track-builder "Glitch hop" 180 128)]
        [fname (+ "glitch-hop-" hash ".it")]
        [notes (get-good-notes rnd 5)]
        [notes-set [notes (mutate-notes rnd notes)]]
        [rootnote (rnd.randint 48 72)]
        [bleep-crunch-sample-set (list-comp (make_bleep_crunch.make-sample-set rnd it sample) [x (range 2)])]
        [bleep-crunch-generated-settings (list-comp (make_bleep_crunch.make-pattern-settings rnd it bleep-crunch-sample-set :rootnote rootnote :notes (get notes-set (int (/ x 2)))) [x (range 4)])]
        [808-sample-sets (list-comp (make_808er.make-sample-set rnd it sample) [x (range 2)])]
        [808-generated-settings (list-comp (make_808er.make-pattern-settings rnd it 808-sample-sets :rootnote rootnote :notes (get notes-set (int (/ x 2)))) [x (range 2)])]
        [patterns (list-comp
                    {:pace (rnd.choice (if (= x 2)
                                         [2 2 4 8]
                                         [2 4]))
                     :pattern (list-comp (rnd.choice (if (= x 2)
                                                       [0 1 1 1 1 1 1]
                                                       [0 1 1 1]))
                                [y (range 2)])}
                    [x (range 3)])]]
    (print fname)
    ;(pr "notes set" notes-set)
    ;(pprint patterns stderr)
    (for [p (range 16)]
      (let [[section (% (int (/ p 8)) 2)]]
        ;(pr "pattern" p)
        ;(pr "section" section)
        (when (play-pattern 0 p patterns)
          (let [[settings (* section 2)]]
            ;(pr "bleep 1 settings" settings)
            (make_bleep_crunch.make-pattern rnd it pattern (get bleep-crunch-generated-settings settings) (get bleep-crunch-sample-set 0) p 0 row-count)))
        (when (play-pattern 1 p patterns)
          (let [[settings (+ (* section 2) 1)]]
            ;(pr "bleep 2 settings" settings)
            (make_bleep_crunch.make-pattern rnd it pattern (get bleep-crunch-generated-settings settings) (get bleep-crunch-sample-set 1) p 1 row-count)))
        (when (or
                (play-pattern 2 p patterns)
                (not (or
                       (play-pattern 0 p patterns)
                       (play-pattern 1 p patterns))))
          ;(pr "playing beat" section)
          (make_808er.make-pattern rnd it pattern (get 808-generated-settings section) (get 808-sample-sets section) p 2 row-count))))
    (add-message it "channels 5")
    (it.save fname)))

(if (= __name__ "__main__")
  (main argv))

