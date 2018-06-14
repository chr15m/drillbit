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

(def channels 2)

(def breakbeat-pattern [1 0 2 0  0 0 2 0])

(defn make-sample-set [rnd it sampler &optional [breaks ["bboy.wav"  "click.wav"  "coldsweat.wav"  "foolsgold.wav"  "funkydrummer.wav"  "worm.wav"]]]
  (let [[break-chunk-count 8]
        [break-name (rnd.choice breaks)]
        [samples-break (sampler "break" (+ "samples/breaks/" break-name) :slices break-chunk-count)]
        [samples-bass (sampler "break-bass" (rnd.choice (dir-sample-list "samples/trap-kicks" "kick")))]
        [samples-snare (sampler "break-snare" (rnd.choice (dir-sample-list "samples/crispy" "crispy")))]]
    [samples-break [samples-bass samples-snare]]))

(defn make-pattern-settings [rnd it sample-set &kwargs _]
  (let [[break-chunk-count 8]
        [length-break-chunk (sample-length it (-> sample-set (get 0) (get 0)))]
        [length-beat (int (* 44100 (/ 60.0 it.tempo)))]
        [break-note (int (ftom (* (mtof 60) (/ length-break-chunk length-beat) (/ break-chunk-count 4))))]
        [break-rhythm (break-pattern-mutate rnd (/ 2 64)
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
    ;(apply-fx-to-pattern (get it.patlist pattern-number) 0 :seed (fn [] (str (rnd.random))))
    
    ))

(defn break-pattern-mutate [rnd probability pattern]
  (list-comp (if (< (rnd.random) probability) (rnd.choice pattern) x) [x pattern]))

