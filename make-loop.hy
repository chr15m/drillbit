#!/usr/bin/env hy

(import
  [os]
  [os [environ]]
  [sys [argv stderr path exit]]
  [random [Random]]
  [pprint [pprint]])

(let [[me (os.path.abspath (os.path.join os.__file__ ".." ".." ".." ".."))]]
  (path.append me)
  (path.append (os.path.join me "drillbit"))
  (path.append (os.path.join me "autotracker")))

(import
  [autotracker.utils [track-builder add-message initial-hash extract-hash make-rng]]
  [autotracker.compose [make-notes-progression]]
  [generators [lookup get-generator-name]])

(require hy.contrib.loop)

(def auto-pan
  {"bleep_crunch" 63
   "noiser" 63
   "dnb" 0
   "808er" 0
   "mad_cow" 0
   "hats" 0
   "break2" 0
   "break" 0
   "hiphop" 0
   "melody" 63
   "vox" 63})

(defn main [argv]
  (if (or
        (< (len argv) 3)
        (not (in (get argv 2) lookup)))
    (usage argv)
    (let [[bpm (int (get argv 1))]
          [hash-song (initial-hash (.get environ "HASH"))]

          [row-count 128]

          [[it sampler pattern-gen] (track-builder "Algorave loop" bpm row-count)]
          [fname (os.path.join (environ.get "DEST" "") (+ "loop-" hash-song ".it"))]

          [progression (make-notes-progression (make-rng hash-song "notes"))]

          ;[[note-sets rootnote pattern] (list-comp (get progression n) [n [:note-sets :rootnote :pattern]])]
          [notes (if (.has_key environ "NOTES")
                   (list-comp (int n) [n (.split (.get environ "NOTES") " ")])
                   (.sample (make-rng hash-song "notes") [0 2 4 5 7 9 11] 4))]
          [rootnote 60]
          [pattern [1 1 1 1]]

          [sections (len pattern)]
          ;[_ (print sections note-sets pattern)]
          ; TODO: write all params & inputs to a file

          [generator-names (slice argv 2)]
          [generators (list-comp (get lookup generator-name) [generator-name generator-names])]
          [sample-sets (list-comp
                         (let [[generator (get generators g)]] (.make-sample-set generator (make-rng hash-song "samples" g generator.__file__) it sampler))
                         [g (range (len generators))])]

          [settings (list-comp
                      (let [[generator (get generators g)]] (.make-pattern-settings generator (make-rng hash-song "pattern" g generator.__file__) it :sample-set (get sample-sets g) :rootnote rootnote :notes notes))
                      [g (range (len generators))])]]

      (for [p (range sections)]
        (loop [[channel 0] [g 0]]
          (let [[generator (get generators g)]
                [rnd-beats (make-rng hash-song "pattern" g generator.__file__ p)]]
            (.make-pattern generator rnd-beats it pattern-gen (get settings g) (get sample-sets g) p channel row-count)
            ; alternate left and right channels
            (for [chnpan (range generator.channels)]
              (setv (get it.chnpan (+ channel chnpan)) (get auto-pan (get-generator-name generator))))
            (when (< g (dec (len generators)))
              (recur (+ generator.channels channel) (inc g))))))

      (print fname)
      (it.save fname)
      (let [[infofile (file (+ fname ".txt") "w")]]
        (print hash-song :file infofile)
        (print (.join " " argv) :file infofile)))))

(defn usage [argv]
  (print "Usage:" (get argv 0) "BPM GENERATOR-1 [GENERATOR-2...]\n")
  (print "Environment variables:")
  (print "\tHASH=...deadbeef\n")
  (print "Generators:")
  (for [k lookup]
    (print "\t" k))
  (exit))

(if (= __name__ "__main__")
  (main argv))
