#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped]]
  [autotracker.it.pattern [empty]]
  [autotracker.compose [genetic-rhythm-loop]]
  [sfxr [make-bleep sfxr-render]]
  [random [Random]]
  [math [sin]]
  [sys [argv stderr]])

(def channels 1)

(defn make-sample-set [rnd it sampler]
  [(sampler "cow" "samples/808-cowbell.wav")])

(defn make-pattern-settings [rnd it sample-set &optional [rootnote 60] [notes [0 5 7]] &kwargs _]
  (let [[loop-length (rnd.choice [8 16 32 64])]]
    (genetic-rhythm-loop rnd loop-length)))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[combined-loop settings]
        [pace 4]
        [sample-cowbell (get sample-set 0)]]
    (pattern pattern-number channel
             (list-comp
               (if (and (not (% r pace)) (get-wrapped combined-loop (int (/ r pace))))
                 [60 sample-cowbell 255 0 0]
                 empty)
               [r (xrange row-count)]))))

