#!/usr/bin/env hy

(import
  [autotracker.utils [track-builder initial-hash extract-hash dir-sample-list get-wrapped add-message prerr]]
  [autotracker.it.pattern [empty]]
  [autotracker.compose [get-good-notes genetic-rhythm-loop make-fractal-note-sequence]]
  [sfxr [make-bleep sfxr-render]]
  [random [Random]]
  [sys [argv stderr]])

(def channels 1)

(defn make-sample-set [rnd it sampler]
  (sampler "sfxr-bleep" (sfxr-render (make-bleep :r rnd) "bleep.wav")) )

(defn shuffled [rnd elements]
  (let [[copy (slice elements)]]
    (rnd.shuffle copy)
    copy))

(defn make-fractal-melody [rnd source-loop length &optional [probability-switch 0.5]]
  (loop [[melody []] [pace 1] [pos 0]]
    (if (< (len melody) length)
      (recur (+ melody
              [(get source-loop (% (* pos pace) (len source-loop)))])
      (min 4
        (max 1
          (if (< (rnd.random) probability-switch) ((rnd.choice [inc dec]) pace) pace)))
      (inc pos))
      melody)))

(defn make-octave-sequence [rnd sequence-length probability]
  (list-comp (if (< (rnd.random) probability) (rnd.choice [-1 1]) 0) [t (xrange sequence-length)]))

(defn make-pattern-settings [rnd it sample-set &optional [notes [0 5 7]] [rootnote 60] [pace 4] [volume 64] [note-length nil] [octave-wander-probability 0.1] [note-end-type :cut] &kwargs _]
  (let [[stretch 4]
        [sparseness 2]
        [scale (shuffled rnd (+ notes (* [None] sparseness)))]
        [note-loop (list-comp (get scale (% s (len scale))) [s (range 8)])]
        [melody (make-fractal-melody rnd note-loop (rnd.choice [8 16 32]) :probability-switch 0.33)]
        [octaves (make-octave-sequence rnd (rnd.choice [8 16 32]) (rnd.random))]]
    ;(prerr "scale:" (sorted (list (set (list-comp s [s scale] (!= s -1))))))
    ;(prerr "melody:" melody)
    (list-comp
      (let [[pos (int (/ n stretch))]
            [note (get-wrapped melody pos)]
            [octave (get-wrapped octaves pos)]]
        (if (or (= note nil) (% n stretch))
          None
          (+ rootnote octave note)))
      [n (range (* stretch (len melody)))])))

(defn make-pattern [rnd it pattern settings sample-set pattern-number channel row-count]
  (let [[note-loop settings]
        [rows (xrange row-count)]]
    ;(prerr "note-loop" note-loop)
    (pattern pattern-number channel
             (list-comp
               (let [[note (get-wrapped note-loop r)]]
                 (if (= note None)
                   empty
                   [note sample-set 32 0 0]))
               ;[(get-wrapped note-loop r) sample-set 32 0 0]
               ;(get-wrapped note-loop r)
               [r rows]))))

