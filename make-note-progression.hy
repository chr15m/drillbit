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
    (print "progression hash" (+ hash ";"))
    (print "progression hash-int" (+ (str (seed-hash-int hash)) ";"))
    (print "progression sections" (+ (str sections) ";"))
    (print "progression beats" (+ (str (* 16 (len (get progression :pattern)))) ";"))
    (print "progression pattern" (+ (str-ints (get progression :pattern)) ";"))
    (print "progression rootnote" (+ (str (get progression :rootnote)) ";"))
    (for [s (range (len note-sets))]
      (let [[notes (get note-sets s)]]
        (print (+ "progression section " (str s)) (+ (str-ints notes) ";"))))))

(defn str-ints [i]
  (.join " " (list-comp (str n) [n i])))

(defn seed-hash-int [seed]
  (& (int "7fffff" 16)
     (int (slice seed 0 6) 16)))

(if (= __name__ "__main__")
  (main argv))

