#!/usr/bin/env hy

(import
  [sys [argv]]
  [autotracker.utils [track-builder initial-hash extract-hash add-message]]
  [autotracker.compose [get-good-notes]]
  [random [Random]]
  [generators [make_bleep_crunch make_808er]])

(defn main [argv]
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sample pattern] (track-builder "Glitch hop" 180 128)]
        [fname (+ "glitch-hop-" hash ".it")]
        [notes (get-good-notes rnd 5)]
        [rootnote (rnd.randint 48 72)]
        [bleep-crunch-sample-set (list-comp (make_bleep_crunch.make-sample-set rnd sample) [x (range 2)])]
        [bleep-crunch-generated-settings (list-comp (make_bleep_crunch.make-pattern-settings rnd rootnote notes bleep-crunch-sample-set) [x (range 2)])]
        [808-sample-set (make_808er.make-sample-set rnd sample)]
        [808-generated-settings (make_808er.make-pattern-settings rnd)]]
    (print fname)
    (for [p (range 4)]
      (make_bleep_crunch.make-pattern rnd pattern (get bleep-crunch-generated-settings 0) (get bleep-crunch-sample-set 0) p 0 row-count rootnote)
      (if (> p 1)
        (make_bleep_crunch.make-pattern rnd pattern (get bleep-crunch-generated-settings 1) (get bleep-crunch-sample-set 1) p 1 row-count rootnote))
      (make_808er.make-pattern rnd pattern 808-generated-settings 808-sample-set p 2 row-count))
    (add-message it "channels 5")
    (it.save fname)))

(if (= __name__ "__main__")
  (main argv))

