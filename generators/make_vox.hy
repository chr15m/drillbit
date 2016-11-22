#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped add-message]]
  [autotracker.it.pattern [empty]]
  [autotracker.compose [get-good-notes genetic-rhythm-loop]]
  [sfxr [make-bleep sfxr-genetics]]
  [random [Random]]
  [math [sin]]
  [os]
  [sys [argv stderr]])

(defn make-sample-set [rnd it sampler]
  (let [[vox-sets (and (os.path.isdir "acapellas") (list-comp d [d (os.listdir "acapellas")] (os.path.isdir (os.path.join "acapellas" d))))]
        [vox-sample-folder (and vox-sets (os.path.join "acapellas" (rnd.choice vox-sets)))]
        [vox-sample-list (and vox-sample-folder (list-comp (os.path.join vox-sample-folder f) [f (os.listdir vox-sample-folder)]))]
        [vox-sample-set (rnd.sample vox-sample-list 30)]]
    (list-comp (sampler (% "vox-%d" s) (get vox-sample-set s)) [s (range (len vox-sample-set))])))

(defn make-pattern-settings [rnd it sample-set &kwargs _]
  (let [[base-pattern (list-comp (rnd.choice (get-wrapped [[0 1 1 1 1 1 1 1 1 1] [0 1 1 1 1]] p)) [p (range 8)])]
        [sample-subset (rnd.sample sample-set 15)]]
    [base-pattern sample-subset]))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[[base-pattern sample-subset] settings]]
    (pattern pattern-number channel
             (loop [[row 0] [pace 4] [result []]]
               (if (< row row-count)
                 ; if we landed on a row we're currently processing
                 (let [[hit (not (% row pace))]]
                   (recur
                     (inc row)
                     (if (and hit (< (rnd.random) 0.33333))
                       (rnd.choice [2 2 2  3  4 4 4 4])
                       pace)
                     (+ result [(if (and hit (get-wrapped base-pattern (/ row pace)))
                                  [60 (rnd.choice sample-subset) 64 0 0]
                                  empty)])))
                 result)))))

(defn main [argv]
  (let [[hash (initial-hash (extract-hash argv))]
        [rnd (Random hash)]
        [row-count 128]
        [[it sample pattern] (track-builder "Vox" 180 128)]
        [fname (+ "vox-" hash ".it")]
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
