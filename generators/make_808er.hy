#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped]]
  [autotracker.it.pattern [empty]]
  [autotracker.tables [beats]]
  [autotracker.fx [apply-fx-to-pattern]]
  [random [Random]]
  [math [sin]]
  [sys [argv stderr]])

(require hy.contrib.loop)

(def channels 3)

(defn make-loop [rnd drum beat-length]
  (let [[hiphop-beat (-> beats (get :hiphop))]]
    (list-comp
      (< (rnd.random) (get-wrapped (get hiphop-beat (keyword drum)) r))
      [r (range beat-length)])))

(defn make-sample-set [rnd it sampler]
  (let [[samples-808-bass (sampler "808-bass" (rnd.choice (dir-sample-list "samples/808" "bass")))]
        [samples-808-snare (list-comp (sampler "808-snare" (rnd.choice (dir-sample-list "samples/808" "snare"))) [x (range 2)])]
        [samples-808-hh [(sampler "808-hat" (rnd.choice (dir-sample-list "samples/808" "hi hat-snappy")))
                         (sampler "808-hat" (rnd.choice (dir-sample-list "samples/808" "hi hat")))]]]
    [samples-808-bass samples-808-snare samples-808-hh]))

(defn make-pattern-settings [rnd it sample-set &optional [rootnote 60] [notes [0 5 7]] &kwargs _]
  (let [[beat-length (rnd.choice [8 16 32])]]
    (print "--- 808er ---" :file stderr)
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
               [r rows]))
    (when (< (rnd.random) 0.125)
      (print "applying drill-n-bass fx" :file stderr)
      (apply-fx-to-pattern (get it.patlist pattern-number) (+ channel 0) :seed (fn [] (str (rnd.random))))
      (apply-fx-to-pattern (get it.patlist pattern-number) (+ channel 1) :seed (fn [] (str (rnd.random))))
      (apply-fx-to-pattern (get it.patlist pattern-number) (+ channel 2) :seed (fn [] (str (rnd.random)))))))

