#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder sample-length initial-hash extract-hash dir-sample-list get-wrapped mtof ftom]]
  [autotracker.it.pattern [empty]]
  [autotracker.tables [beats]]
  [autotracker.fx [apply-fx-to-pattern]]
  [random [Random]]
  [math [sin]]
  [sys [argv]])

(require hy.contrib.loop)

(def breakbeat-pattern [1 0 2 0  0 0 2 0])

(defn make-sample-set [rnd it sampler]
  (let [[break-chunk-count 8]
        [break-name (rnd.choice ["amen.wav" "think.wav"])]
        [samples-break (sampler "break" (+ "samples/" break-name) :slices break-chunk-count)]
        [samples-bass (sampler "break-bass" (rnd.choice (dir-sample-list "samples/trap-kicks" "kick")))]
        [samples-snare (sampler "break-snare" (rnd.choice (dir-sample-list "samples/crispy" "crispy")))]]
    [samples-break [samples-bass samples-snare]]))

(defn make-pattern-settings [rnd it sample-set &kwargs _]
  (let [[break-chunk-count 8]
        [length-break-chunk (sample-length it (-> sample-set (get 0) (get 0)))]
        [length-beat (int (* 44100 (/ 60.0 it.tempo)))]
        [break-note (int (ftom (* (mtof 60) (/ length-break-chunk length-beat) (/ break-chunk-count 4))))]
        [break-rhythm (break-pattern-mutate rnd (/ 2 16)
                                            (rnd.choice [[0 1 2 3  4 5 6 7  8 9 10 11  12 13 14 15]
                                                         [0 1 2 0  1 2 3 4  8 9 10  8   9 10 13 15]]))]]
    {:note break-note
     :break break-rhythm}))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[break-rhythm (get settings :break)]
        [break-note (get settings :note)]
        [[samples-break samples-bass-snare] sample-set]
        [pace 4]
        [rows (xrange row-count)]]
    (pattern pattern-number (+ channel 0)
             (list-comp
               (if (not (% r pace))
                 [break-note (get-wrapped samples-break (get-wrapped break-rhythm (int (/ r pace)))) 64 0 0]
                 empty)
               [r rows]))
    (pattern pattern-number (+ channel 1)
             (list-comp
               (let [[tick (int (/ r pace))]
                     [drum-type (get-wrapped breakbeat-pattern (get-wrapped break-rhythm tick))]]
                 (if (and (not (% r pace)) drum-type)
                   [60 (get samples-bass-snare (- drum-type 1)) 64 0 0]
                   empty))
               [r rows]))
    (for [c [0 1]]
      (apply-fx-to-pattern (get it.patlist pattern-number) (+ channel c) :seed (fn [] (str (rnd.random)))))))

(defn break-pattern-mutate [rnd probability pattern]
  (list-comp (if (< (rnd.random) probability) (rnd.choice pattern) x) [x pattern]))

(defn main [argv]
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sampler pattern] (track-builder "breakbeat" 180 128)]
        [fname (+ "breakbeat-" hash ".it")]
        [sample-set (make-sample-set rnd sampler)]
        [generated-settings (make-pattern-settings rnd it sample-set)]]
    (print fname)
    (for [p (range 4)]
      (make-pattern rnd pattern generated-settings sample-set p 0 row-count))
    (it.save fname)))

(if (= __name__ "__main__")
  (main argv))
