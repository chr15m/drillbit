#!/usr/bin/env hy

(import
  [os [environ]]
  [sys [argv stderr path exit]]
  [random [Random]]
  [pprint [pprint]])

(path.append "drillbit")

(import
  [autotracker.utils [track-builder add-message initial-hash extract-hash]]
  [autotracker.compose [make-notes-progression]]
  [generators [lookup]])

(defn main [argv]
  (if (or
        (< (len argv) 2)
        (not (in (get argv 1) lookup)))
    (usage argv)
    (let [[generator-name (get argv 1)]
          [generator (get lookup generator-name)]
          [hash-notes (initial-hash (extract-hash argv 0))]
          [hash-beats (initial-hash (extract-hash argv 1))]
          [rnd-notes (Random hash-notes)]
          [rnd-beats (Random hash-beats)]
          [row-count 128]
          [[it sampler pattern-gen] (track-builder "Algorave stem" 180 row-count)]
          [fname (+ "stem-" generator-name "-" hash-notes "-" hash-beats ".it")]
          [progression (make-notes-progression rnd-notes)]
          [[note-sets rootnote pattern] (list-comp (get progression n) [n [:note-sets :rootnote :pattern]])]
          [sections (len pattern)]
          [sample-set (generator.make-sample-set rnd-beats sampler)]
          ;[_ (print sections note-sets pattern)]
          [settings (list-comp
                      (generator.make-pattern-settings rnd-beats rootnote (get note-sets s) sample-set)
                      [s (range (len note-sets))])]]
      (for [p (range sections)]
        (generator.make-pattern rnd-beats pattern-gen (get settings (get pattern p)) sample-set p 0 row-count))
      (print fname)
      (it.save fname))))

(defn usage [argv]
  (print "Usage:" (get argv 0) "GENERATOR [NOTE-PROGRESSION-HASH] [GENERATOR-HASH]")
  (print "Generators:")
  (for [k lookup]
    (print "\t" k))
  (exit 1))

(if (= __name__ "__main__")
  (main argv))
