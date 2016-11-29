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
  (let [[pattern-length (rnd.choice [16 32 64])]
        [probability-table (rnd.choice [[2 2 4 4 6 8 8] [4 8] [2 2 2 3 4 4 4] [4 4 6 8 8 16]])]
        [base-pattern (list-comp (not (% x (rnd.choice probability-table))) [x (range pattern-length)])]
        [sample-subset (rnd.sample sample-set 20)]]
    ;(stderr.write (+ "probability table: " (str probability-table) "\n"))
    ;(stderr.write (+ "pattern-length: " (str pattern-length) "\n"))
    [base-pattern sample-subset]))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[[base-pattern sample-subset] settings]]
    (pattern pattern-number channel
             (list-comp (get [empty [60 (rnd.choice sample-subset) 64 0 0]] (get-wrapped base-pattern x)) [x (range row-count)]))))

