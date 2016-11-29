#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped]]
  [autotracker.it.pattern [empty]]
  [autotracker.tables [beats]]
  [random [Random]]
  [math [sin]]
  [sys [argv]])

(require hy.contrib.loop)

(defn make-loop [rnd drum beat-length]
  (let [[hiphop-beat (-> beats (get :hiphop))]]
    (list-comp
      (< (rnd.random) (get-wrapped (get hiphop-beat (keyword drum)) r))
      [r (range beat-length)])))

(defn make-sample-set [rnd it sampler]
  (let [[samples-808-bass (sampler "hiphop-bass-drum" (rnd.choice (dir-sample-list "samples/CanOfBeats" "bd")))]
        [samples-808-snare (list-comp (sampler "hiphop-snare-drum" (rnd.choice (dir-sample-list "samples/CanOfBeats" "sd"))) [x (range 2)])]
        [samples-808-hh [(sampler "hiphop-hat" (rnd.choice (dir-sample-list "samples/CanOfBeats" "hh")))
                         (sampler "hiphop-hat" (rnd.choice (dir-sample-list "samples/CanOfBeats" "hh")))]]]
    [samples-808-bass samples-808-snare samples-808-hh]))

(defn make-pattern-settings [rnd it sample-set &optional [rootnote 60] [notes [0 5 7]] &kwargs _]
  (let [[beat-length (rnd.choice [8 16 32])]]
    {:bd (make-loop rnd 'bd beat-length)
     :sd (make-loop rnd 'sd beat-length)
     :hh (make-loop rnd 'hh beat-length)
     :ho (list-comp (< (rnd.random) 0.05) [r (range beat-length)])}))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[loops settings]
        [[samples-808-bass samples-808-snare samples-808-hh] sample-set]
        [pace 4]
        [rows (xrange row-count)]]
    (pattern pattern-number (+ channel 0)
             (list-comp
               (if (and (not (% r pace)) (get-wrapped (get loops :bd) (int (/ r pace))))
                 [60 samples-808-bass 255 0 0]
                 empty)
               [r rows]))
    (pattern pattern-number (+ channel 1)
             (list-comp
               (if (and (not (% r pace)) (get-wrapped (get loops :sd) (int (/ r pace))))
                 [60 (get samples-808-snare 0) 255 0 0]
                 empty)
               [r rows]))
    (pattern pattern-number (+ channel 2)
             (list-comp
               (if (and (not (% r pace)) (get-wrapped (get loops :hh) (int (/ r pace))))
                 [60 (get samples-808-hh (int (or (get-wrapped (get loops :ho) (int (/ r pace))) (> (rnd.random) 0.95)))) 32 0 0]
                 empty)
               [r rows]))))

