(import random)

(require hy.contrib.loop)

(import [utils [value-or-callable flip-pattern-data weighted-choice fx-code]])

(defn replace-array-section [a i v]
  "replace from index i in array a with new contents v"
    (+ (slice a 0 i) v (slice a (+ i (len v)))))

(defn fx-chopper [chunk r]
  ; choose a random sequence of note-cuts for each beat in the chunk
  (let [[seqs (list-comp (r.choice [[0 255 255 255]
                                    [64 0 255 255]
                                    [64 0 64 0]
                                    [64 255 0 255]]) [p (range (int (/ (len chunk) 4)))])]]
    ; return a modified version of the chunk with the volume columns updated
    (list-comp
      (let [[i (% c 4)]
            [o (int (/ c 4))]]
        (replace-array-section (get chunk c) 2 [(get (get seqs o) i)]))
      [c (range (len chunk))])))

(defn fx-reverse [chunk r]
  (list-comp
    (if (= (% c 4) 0)
      (replace-array-section (get chunk c) 3 [(fx-code "S") (int "0x9F" 16)])
      (get chunk c)) [c (range (len chunk))]))

(defn fx-retrigger [chunk r]
  (let [[note (get chunk 0)]
        [rate (weighted-choice r {1 3 2 3 3 1})] ; more likely to pick 1 or 2 than 3
        [volume-ramp (or (> (len chunk) 8) (< (r.random) 0.5))]]
    (list-comp
      (let [[row-value
             (if (= (% c rate) 0)
               note
             (get chunk c))]]
        (if volume-ramp
          (replace-array-section row-value 2 [(int (* 64 (/ (- (len chunk) c) (len chunk))))])
          row-value)) [c (range (len chunk))])))

(defn apply-fx-to-pattern [pattern column-number &optional
                           [seed random.random] [probability 0.125] [fx-probabilities {fx-chopper 50 fx-reverse 10 fx-retrigger 50}]]
  (let [[rnd (random.Random (value-or-callable seed))]
        [pattern-data (flip-pattern-data (. pattern data))]
        [column (get pattern-data column-number)]]
    (loop [[r 0] [new-pattern-data pattern-data] [new-column column]]
      (if (< r (len new-column))
        (let [[chunk (slice new-column r (+ r (* (rnd.randint 1 3) 4)))]
              [replaced-chunk (if (< (rnd.random) probability) ((weighted-choice rnd fx-probabilities) chunk rnd) chunk)]
              [replaced-column (replace-array-section new-column r replaced-chunk)]
              [replaced-pattern-data (replace-array-section new-pattern-data column-number [replaced-column])]]
          (recur (+ r 4) replaced-pattern-data replaced-column))
        (setv pattern.data (flip-pattern-data new-pattern-data))))))

(defn apply-drop-groups-to-pattern [pattern pattern-number drop-groups never-drop &optional [seed random.random] [section-size 4]]
  "drop groups of instruments together - build up / break down"
  (let [[section (int (/ pattern-number section-size))]
        [compiled-seed (+ (str (value-or-callable seed)) "-" (str section))]
        [rnd (random.Random compiled-seed)]
        [drop-length (if (= section 0) 4 (rnd.choice [2 4]))]
        [drop-group (if (< (rnd.random) 0.1)
                      ; unseeded - changes every pattern
                      (random.choice drop-groups)
                      ; seeded - constant through section
                      (rnd.choice drop-groups))]]
    (when (and (or (= section 0) (< (rnd.random) 0.33)) (< (% pattern-number section-size) drop-length))
      (setv pattern.data
        (list-comp 
          (list-comp
            (if (or (in col never-drop) (in col drop-group))
              [253 0 255 0 0]
              (get row col))
            [col (range (len row))])
          [row pattern.data])))))

