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

(defn make-sample-set [rnd sample]
  (let [[samples-808-bass (sample "808-bass" (rnd.choice (dir-sample-list "samples/808" "bass")))]
        [samples-808-snare (list-comp (sample "808-snare" (rnd.choice (dir-sample-list "samples/808" "snare"))) [x (range 2)])]
        [samples-808-hh [(sample "808-hat" (rnd.choice (dir-sample-list "samples/808" "hi hat-snappy")))
                         (sample "808-hat" (rnd.choice (dir-sample-list "samples/808" "hi hat")))]]]
    [samples-808-bass samples-808-snare samples-808-hh]))

(defn make-pattern-settings [rnd]
  (let [[beat-length (rnd.choice [8 16 32])]]
    {:bd (make-loop rnd 'bd beat-length)
     :sd (make-loop rnd 'sd beat-length)
     :hh (make-loop rnd 'hh beat-length)
     :ho (list-comp (< (rnd.random) 0.05) [r (range beat-length)])}))

(defn make-pattern [rnd pattern settings sample-set pattern-number channel row-count]
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

(defn main [argv]
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sample pattern] (track-builder "808 hiphop" 180 128)]
        [fname (+ "808s-" hash ".it")]
        [sample-set (make-sample-set rnd sample)]
        [generated-settings (make-pattern-settings rnd)]]
    (print fname)
    (for [p (range 4)]
      (make-pattern rnd pattern generated-settings sample-set p 0 row-count))
    (it.save fname)))

(if (= __name__ "__main__")
  (main argv))
