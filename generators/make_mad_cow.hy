#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped]]
  [autotracker.it.pattern [empty]]
  [autotracker.compose [genetic-rhythm-loop]]
  [sfxr [make-bleep sfxr-render]]
  [random [Random]]
  [math [sin]]
  [sys [argv stderr]])

(defn make-sample-set [rnd sample]
  [(sample "cow" "samples/808-cowbell.wav")])

(defn make-pattern-settings [rnd]
  (let [[loop-length (rnd.choice [8 16 32 64])]]
    (genetic-rhythm-loop rnd loop-length)))

(defn make-pattern [rnd pattern settings sample-set pattern-number channel row-count]
  (let [[combined-loop settings]
        [pace 4]
        [sample-cowbell (get sample-set 0)]]
    (pattern pattern-number channel
             (list-comp
               (if (and (not (% r pace)) (get-wrapped combined-loop (int (/ r pace))))
                 [60 sample-cowbell 255 0 0]
                 empty)
               [r (xrange row-count)]))))

(defn main [argv]
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sample pattern] (track-builder "808 cow" 180 row-count)]
        [fname (+ "cowbell-" hash ".it")]
        [sample-set (make-sample-set rnd sample)]
        [generated-settings (make-pattern-settings rnd)]]
    (print fname)
    (for [p (range 4)]
      (make-pattern rnd pattern generated-settings sample-set p 0 row-count))
    (it.save fname)))

(if (= __name__ "__main__")
  (main argv))
