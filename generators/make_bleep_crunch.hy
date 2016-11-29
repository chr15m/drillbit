#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped add-message]]
  [autotracker.it.pattern [empty]]
  [autotracker.compose [get-good-notes genetic-rhythm-loop]]
  [sfxr [make-bleep sfxr-render]]
  [random [Random]]
  [math [sin]]
  [sys [argv stderr]])

(defn make-sample-set [rnd it sampler]
  (list-comp (sampler "sfxr-bleep" (sfxr-render (make-bleep :r rnd) "bleep.wav")) [s (range 3)]))

(defn make-pattern-settings [rnd it sample-set &optional [notes [0 5 7]] [rootnote 60] &kwargs _]
  (let [[note-loop (list-comp (+ (rnd.choice notes) rootnote) [l (range (rnd.choice [8 16 32 64]))])]
        [samples-loop (list-comp (rnd.choice sample-set) [l (range (rnd.choice [8 16 32 64]))])]
        [rhythm-loop (genetic-rhythm-loop rnd (rnd.choice [16 32 64]))]]
    [note-loop samples-loop rhythm-loop]))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[[note-loop samples-loop rhythm-loop] settings]
        [pace 4]
        [rows (xrange row-count)]]
    (pattern pattern-number channel
             (list-comp
               (if (and (not (% r pace)) (get-wrapped rhythm-loop (int (/ r pace))))
                 [(get-wrapped note-loop r) (get-wrapped sample-set r) 32 0 0]
                 empty)
               [r rows]))))
