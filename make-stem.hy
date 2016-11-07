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
        (< (len argv) 3)
        (not (in (get argv 1) lookup)))
    (usage argv)
    (let [[generator-name (get argv 1)]
          [generator (get lookup generator-name)]
          [bpm (int (get argv 2))]
          [hash-notes (initial-hash (extract-hash argv 0))]
          [hash-beats (initial-hash (extract-hash argv 1))]
          [rnd-notes (Random hash-notes)]
          [rnd-beats (Random hash-beats)]
          [row-count 128]
          [[it sampler pattern-gen] (track-builder "Algorave stem" bpm row-count)]
          [fname (+ "stem-" generator-name "-" hash-notes "-" hash-beats ".it")]
          [progression (make-notes-progression rnd-notes)]
          [[note-sets rootnote pattern] (list-comp (get progression n) [n [:note-sets :rootnote :pattern]])]
          [sections (len pattern)]
          [sample-set (generator.make-sample-set rnd-beats it sampler)]
          ;[_ (print sections note-sets pattern)]
          [settings (list-comp
                      (generator.make-pattern-settings rnd-beats it :sample-set sample-set :rootnote rootnote :notes (get note-sets s))
                      [s (range (len note-sets))])]]
      (for [p (range sections)]
        (generator.make-pattern rnd-beats it pattern-gen (get settings (get pattern p)) sample-set p 0 row-count))
      (print fname)
      (it.save fname))))

(defn usage [argv]
  (print "Usage:" (get argv 0) "GENERATOR BPM [NOTE-PROGRESSION-HASH] [GENERATOR-HASH]")
  (print "Generators:")
  (for [k lookup]
    (print "\t" k))
  (exit 1))

(if (= __name__ "__main__")
  (main argv))
