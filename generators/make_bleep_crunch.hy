#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped add-message]]
  [autotracker.it.pattern [empty]]
  [autotracker.compose [get-good-notes genetic-rhythm-loop]]
  [sfxr [make-bleep sfxr-render]]
  [random [Random]]
  [math [sin]]
  [sys [argv stderr]])

(defn make-sample-set [rnd sample]
  (list-comp (sample "sfxr-bleep" (sfxr-render (make-bleep :r rnd) "bleep.wav")) [s (range 3)]))

(defn make-pattern-settings [rnd rootnote notes sample-set]
  (let [[note-loop (list-comp (+ (rnd.choice notes) rootnote) [l (range (rnd.choice [8 16 32 64]))])]
        [samples-loop (list-comp (rnd.choice sample-set) [l (range (rnd.choice [8 16 32 64]))])]
        [rhythm-loop (genetic-rhythm-loop rnd (rnd.choice [16 32 64]))]]
    [note-loop samples-loop rhythm-loop]))

(defn make-pattern [rnd pattern settings sample-set pattern-number channel row-count]
  (let [[[note-loop samples-loop rhythm-loop] settings]
        [pace 4]
        [rows (xrange row-count)]]
    (pattern pattern-number channel
             (list-comp
               (if (and (not (% r pace)) (get-wrapped rhythm-loop (int (/ r pace))))
                 [(get-wrapped note-loop r) (get-wrapped sample-set r) 32 0 0]
                 empty)
               [r rows]))))

(defn main [argv]
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sample pattern] (track-builder "Beep crunch" 180 128)]
        [fname (+ "beep-crunch-" hash ".it")]
        [notes (get-good-notes rnd 5)]
        [rootnote (rnd.randint 48 72)]
        [sample-set (make-sample-set rnd sample)]
        [generated-settings (make-pattern-settings rnd rootnote notes sample-set)]]
    (print fname)
    (for [p (range 4)]
      (make-pattern rnd pattern generated-settings sample-set p 0 row-count rootnote))
    (it.save fname)))

(if (= __name__ "__main__")
  (main argv))
