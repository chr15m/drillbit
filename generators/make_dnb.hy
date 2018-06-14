#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped]]
  [autotracker.it.pattern [empty]]
  [autotracker.tables [beats]]
  [random [Random]]
  [math [sin]]
  [sys [argv]])

(require hy.contrib.loop)

(def channels 2)

(defn make-loop [rnd drum beat-length]
  (let [[beat (get beats (rnd.choice [:dnb-1 :dnb-2]))]]
    (list-comp
      (< (rnd.random) (get-wrapped (get beat (keyword drum)) r))
      [r (range beat-length)])))

(defn make-sample-set [rnd it sampler]
  (let [[sample-bass (sampler "dnb-bass-drum" (rnd.choice (dir-sample-list "samples/trap-kicks" "kick")))]
        [samples-snare (list-comp (sampler "dnb-snare-drum" (rnd.choice (dir-sample-list "samples/crispy" "crispy"))) [x (range 2)])]]
    [sample-bass samples-snare]))

(defn make-pattern-settings [rnd it sample-set &optional [rootnote 60] [notes [0 5 7]] &kwargs _]
  (let [[beat-length (rnd.choice [16 32])]]
    {:bd (make-loop rnd 'bd beat-length)
     :sd (make-loop rnd 'sd beat-length)}))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[loops settings]
        [[sample-bass samples-snare] sample-set]
        [pace 2]
        [rows (xrange row-count)]]
    (pattern pattern-number (+ channel 0)
             (list-comp
               (let [[p (int (/ r pace))]]
                 (if (and (not (% r pace)) (get-wrapped (get loops :bd) p))
                   [60 sample-bass (get [255 32] (% p 2)) 32 0 0]
                   empty))
               [r rows]))
    (pattern pattern-number (+ channel 1)
             (list-comp
               (let [[p (int (/ r pace))]]
                 (if (and (not (% r pace)) (get-wrapped (get loops :sd) p))
                   [60 (get samples-snare 0) (get [255 32] (% p 2)) 0 0]
                   empty))
               [r rows]))))

