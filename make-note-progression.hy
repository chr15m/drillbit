#!/usr/bin/env hy

(import
  [sys [argv]]
  [random [Random]]
  [autotracker.utils [initial-hash extract-hash]]
  [autotracker.compose [make-notes-progression]])

(require hy.contrib.loop)

(defn main [argv]
  (let
    [[hash (initial-hash (extract-hash argv))]
     [rnd (Random hash)]
     [progression (make-notes-progression rnd)]
     [note-sets (get progression :note-sets)]
     [sections (len note-sets)]]
    (print "hash" (+ hash ";"))
    (print "sections" (+ (str sections) ";"))
    (print "pattern" (+ (str-ints (get progression :pattern)) ";"))
    (print "rootnote" (+ (str (get progression :rootnote)) ";"))
    (for [s (range (len note-sets))]
      (let [[notes (get note-sets s)]]
        (print (+ "set-" (str s)) (+ (str-ints notes) ";"))))))

(defn str-ints [i]
  (.join " " (list-comp (str n) [n i])))

(if (= __name__ "__main__")
  (main argv))

