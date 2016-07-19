#!/usr/bin/env hy

; TODO: 
; * shuffle pyramid fx
; * bridge groups build-up/break-down

(import os)
(import sys)
(import math)
(import json)
(import random)
(import [pprint [pprint]])
(import [functools [partial]])

(require hy.contrib.loop)

(import [autotracker [generate MIDDLE_C samples]])
(import [autotracker.it.sample [Sample_File Sample_FileSlice Sample_KS]])
(import [autotracker.it.pattern [empty]])
(import [autotracker.keys [Key_Minor Key_Major]])
(import [autotracker.strategies [Strategy_Main]])
(import [autotracker.generators [Generator_Bass Generator_ProbabilityTable Generator_Callback Generator_AmbientMelody Generator_Breaks]])

(import [utils [print-through get-random-sample get-random-bleep ftom mtof get-wrapped value-or-callable dir-to-samples here add-itf-message-line generator-wrapper]])

(import [sfxr [sfxr-genetics sfxr-render]])
(import [fx [apply-fx-to-pattern apply-drop-groups-to-pattern]])
(import [tables [beats random-squared]])

(def breakbeat-pattern [1 0 2 0  0 0 2 0])

(defn make-fractal-note-sequence [sequence-length number-of-notes &optional [basic-sequence-length 4] [sparseness-probability 0.75]]
 (let [[basic-sequence (random.sample (range number-of-notes) basic-sequence-length)]]
    (list-comp
      (if (> (random.random) sparseness-probability)
        nil
        (get basic-sequence (% (int (/ t (random.choice [1 2 4]))) (len basic-sequence))))
      [t (xrange sequence-length)])))

(defn make-octave-sequence [sequence-length probability]
  (list-comp (if (< (random.random) probability) (random.choice [-1 1]) 0) [t (xrange sequence-length)]))

(defn make-pass-fn []
  (generator-wrapper
    (fn [pattern-index channel-number block]
      (* [empty] (len block)))))

(defn make-hats-fn [hat-samples &optional [volume 64] [hat-probability 0.90] [hat-frequency 4] [trill-probability 0.0625]]
  (let [[pitch-map (dict (list-comp (, h (random.randint 60 72)) [h hat-samples]))]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        (when (not (% row hat-frequency))
          (let [[sample (if (> (random.random) 0.9) (random.choice hat-samples) (get hat-samples 0))]
                [pitch (get pitch-map sample)]]
            (when (< (random.random) hat-probability)
              (setv (get (get pattern.data row) channel-number) 
                [pitch sample (value-or-callable volume row) 0 0])
              ; hat trill
              (when (< (random.random) trill-probability)
                (let [[trill-width (random.choice [1 1 2 2 3])]
                      [trill-length (random.choice [3 4 6 8])]]
                  (for [row-trill (xrange row (min (+ row trill-length) (len pattern.data)) trill-width)]
                    (setv (get (get pattern.data row-trill) channel-number) 
                      [pitch sample (value-or-callable volume row true) 0 0])))))))))))

(defn make-melody-fn [sample root-note sequence octave-sequence notes-set &optional [pace 4] [volume 64] [note-length None] [octave-wander-probability 0.1] [note-end-type :cut]]
  (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
    ; (print strategy.pat_idx)
    (for [row (xrange beat-begin (+ beat-begin beats-length))]
      (let [[current-pace (value-or-callable pace strategy.pat_idx row)]
            [current-note-length (or (value-or-callable note-length) current-pace)]
            [current-octave (or
                              (if (< (random.random) octave-wander-probability) (random.choice [-1 1]) 0)
                              (get-wrapped octave-sequence (int (/ row current-pace))))]
            [note (get sequence (% (int (/ row current-pace)) (len sequence)))]
            [note-end (+ row current-note-length)]]
        (when (and (not (or (= note nil) (= (get notes-set note) nil)))
                ; if we're on the right row division for this pace
                (= (% row current-pace) 0))
          (setv (get (get pattern.data row) channel-number)
            [(+ (get notes-set note) (+ root-note (* 12 current-octave))) sample volume 0 0])
          ; note stop (if inside the pattern)
          (when (< note-end (len pattern.data))
            (setv (get (get pattern.data (+ row current-note-length)) channel-number)
              [(if (= note-end-type :cut) 254 255) 0 127 0 0])))))))

(defn break-pattern-mutate [r probability pattern]
  (list-comp (if (< (r.random) probability) (r.choice pattern) x) [x pattern]))

(defn make-breaks-fn [sample-chunks-break sample-bassdrum sample-snaredrum &optional
                      [break-pitch 60] [break-pace 4] [beat-pace 4] [match-beat true] [seed random.random]]
  (let [[r (random.Random (value-or-callable seed))]
        [bass-snare-rhythm (r.choice [[1 0 2 0  0 1 2 0]
                                      [0 1 2 0  1 0 2 0]
                                      [1 0 0 1  0 0 2 0]
                                      [1 0 2 1  0 2 0 0]])]
        [break-rhythm (break-pattern-mutate r (/ 2 16)
                                            (r.choice [[0 1 2 3  4 5 6 7  8 9 10 11  12 13 14 15]
                                                       [0 1 2 0  1 2 3 4  8 9 10 8  9 10 13 15]]))]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        (let [[tick (/ row break-pace)]
              [break-match (% row break-pace)]]
          ; lay down the breakbeat
          (if (= break-match 0)
            (setv (get (get pattern.data row) channel-number)
              [break-pitch (get-wrapped sample-chunks-break (get-wrapped break-rhythm tick)) 64 0 0]))
          ; lay down the back-beat
          (if match-beat
            ; backbeat should match the break
            (if (= break-match 0)
              (let [[drum-type (get-wrapped breakbeat-pattern (get-wrapped break-rhythm tick))]]
                (when drum-type
                  (setv (get (get pattern.data row) (+ channel-number 1))
                    [60 (get [sample-bassdrum sample-snaredrum] (- drum-type 1)) 64 0 0]))))
            ; backbeat should be original
            (if (= (% row beat-pace) 0)
              (let [[which-drum (get-wrapped bass-snare-rhythm (/ row beat-pace))]]
                (if (> which-drum 0)
                  (setv (get (get pattern.data row) (+ channel-number 1))
                    [60 (get [sample-bassdrum sample-snaredrum] (- which-drum 1)) 64 0 0]))))))))))

(defn make-random-placement-fn [sample-set &optional [seed random.random] [volume 64] [probability 0.75] [number-per-pattern-segment 1]]
  (let [[r (random.Random seed)]
        [positions (list-comp (r.randint 0 16) [x (range 16)])]
        [chances (list-comp (r.random) [x (range 16)])]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [n (range number-per-pattern-segment)]
        (when (< (get-wrapped chances (/ beat-begin beats-length)) probability)
          (let [[row (+ beat-begin (% (* (get-wrapped positions (/ beat-begin beats-length)) 4) beats-length))]
                [sample (get-wrapped sample-set (int (/ beat-begin beats-length)))]]
            (setv (get (get pattern.data row) channel-number)
              [60 sample volume 0 0])))))))

(defn make-probability-table-fn [sample &optional [seed random.random] [probability-table []] [pitch 60] [pace 4] [trippy false]]
  (let [[r (random.Random (value-or-callable seed))]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        (when (and (< (r.random) (get-wrapped probability-table (/ row pace)))
                (or trippy (= (% row pace) 0)))
          (setv (get (get pattern.data row) channel-number) [pitch sample (if (= (% (/ row pace) 2) 0) 64 32) 0 0]))))))

(defn make-vox-fn [random-sample-fn &optional [seed random.random]]
  (let [[r (random.Random (value-or-callable seed))]
        [base-pattern (list-comp (r.choice (get-wrapped [[0 1 1 1 1] [0 1 1]] p)) [p (range 8)])]]
    (generator-wrapper
      (fn [pattern-index channel-number block]
        (loop [[row 0] [pace 4] [result []]]
          (if (< row (len block))
            ; if we landed on a row we're currently processing
            (let [[hit (not (% row pace))]]
              (recur
                (inc row)
                (if (and hit (< (r.random) 0.33333))
                  (r.choice [1 1  2 2 2  3  4 4 4 4])
                  pace)
                (+ result [(if (and hit (get-wrapped base-pattern (/ row pace)))
                           [60 (random-sample-fn) 64 0 0]
                           empty)])))
            result))))))

; eyeballed
(def note-jump-probabilities [5 5 5 5 5 7 7 7 3 3 3 6 6 2 2 4 4 1])

(defn get-good-notes [n]
  ; (random.sample (range 0 12) (+ (max sequence) 1))
  (loop [[c (dec n)] [notes [(random.randint 0 12)]]]
    (if (> c 0)
      (recur
        (dec c)
        (+ notes [(% (+ (get notes -1) (random.choice note-jump-probabilities)) 12)]))
      (sorted notes))))

(defn transform-notes-flip [notes]
  (let [[pivot (random.randint 0 12)]]
    (list-comp (% (+ (* (- n pivot) -1) pivot) 12) [n notes])))

(defn transform-notes-multiply [notes]
  (let [[multiplier (random.randint 0 12)]]
    (list-comp (% (* multiplier n) 12) [n notes])))

(defn make-section-lookup-fn [section-fns sections-pattern]
  (fn [channel-number pattern strategy &rest args]
    (apply
      (get section-fns (get-wrapped sections-pattern strategy.pat_idx))
      (+ [channel-number pattern strategy] (list args)))))

(defn make-octave-noodler-fn []
  ; always return the same result for a particular lookup
  (let [[seed (random.random)]]
    (fn [pattern row]
      (let [[r (random.Random)]]
        (r.seed (.format "octave-noodler-{0}-{1}" seed (int (/ row 8))))
        (random.choice [0 0 0 0 1 -1 -2])))))

(defn make-pace-noodler-fn []
  (let [[noodles (sum (list-comp ((fn [p] (* [p] (min p 4))) (random.choice [1 2 8])) [x (range 1024)]) [])]]
    (fn [pattern row]
      (get-wrapped noodles (+ (* pattern 128) row)))))

; sample sets
(defn get-samples-weirdos [itf sample-count]
  (list-comp
    (itf.smp_add (Sample_File :name (+ "weird-evolved-" (str s))
                              :filename (sfxr-genetics "./sfxrs/" (+ "weird-" (str s)))))
    [s (range sample-count)]))

(defn make-bleep-sample [range &optional [r (random.Random)]]
  (let [[pattern
         {"oldParams" true
          "wave_type" (r.choice [0 1])
          "p_env_attack" (if (r.choice [0 0 0 0 1]) (* 0.5 (r.random)) 0)
          "p_env_sustain" (r.random)
          "p_env_punch" (r.random)
          "p_env_decay" (+ (* (r.random) 0.9) 0.1)
          "p_base_freq" (if (= range "hi") 0.2477 0.1737)
          "p_freq_limit" 0
          "p_freq_ramp" 0
          "p_freq_dramp" 0
          "p_vib_strength" 0
          "p_vib_speed" 0
          "p_arp_mod" (r.choice [0 0 0 0 -0.3162 0.7454 0.7454])
          "p_arp_speed" (+ (* 0.5 (r.random)) 0.4)
          "p_duty" (r.random)
          "p_duty_ramp" (if (r.choice [0 0 1]) (r.random) 0)
          "p_repeat_speed" 0
          "p_pha_offset" 0
          "p_pha_ramp" 0
          "p_lpf_freq" (r.choice [1 (* (r.random) (r.random))])
          "p_lpf_ramp" (- (* (r.random) 2) 1)
          "p_lpf_resonance" (r.random)
          "p_hpf_freq" (r.choice [0 0 0 (r.random)])
          "p_hpf_ramp" (r.choice [0 0 0 (r.random)])
          "sound_vol" 0.5
          "sample_rate" 44100
          "sample_size" 8}]]
    (print (json.dumps pattern)) pattern))

(defn choose-bleep-sample [itf range]
  (let [[bleep-style (random.choice [0 0 0 0 0 0 0 0 0 1 2])]]
    (print "Bleep style:" (get ["sfxr" "evolved" "c64"] bleep-style) range)
    ; jsfxr make-bleep type
    (cond
      [(= bleep-style 0) [(itf.smp_add (Sample_File :name (+ "sfxr-bleep-" range) :filename (sfxr-render (make-bleep-sample range :r (random.Random (random.random))) (+ "samples/sfxr-bleep-" range ".wav")))) 0]]
      ; evolved type
      [(= bleep-style 1) [(itf.smp_add (Sample_File :name (+ "sfxr-evolved-bass-" range) :filename (sfxr-genetics "./sfxr-bass/" (+ "sfxr-evolved-bass-" range)))) (get {"hi" 12 "lo" 0} range)]]
      ; chip type
      [(= bleep-style 2) [(itf.smp_add (Sample_File :name "hi-bleep" :filename (get-random-bleep "hi") :loop "sustain")) (get {"hi" 0 "lo" -12} range)]])))

(defn choose-vox-sample [itf sample-list store r]
  (let [[random-sample (r.choice sample-list)]]
    (if (in random-sample store)
      (get store random-sample)
      (let [[loaded-sample (itf.smp_add (Sample_File :name (os.path.basename random-sample) :filename random-sample))]]
        (assoc store random-sample loaded-sample)
        loaded-sample))))

(defn main [argv]
  (generate
    argv
    (fn [itf seed filename seed-hash]
      (setv itf.tempo (random.randint 175 185))
      (print itf.tempo "BPM")
      (let [[vox-sets (list-comp d [d (os.listdir "acapellas")] (os.path.isdir (os.path.join "acapellas" d)))]
            [vox-sample-folder (os.path.join "acapellas" (random.choice vox-sets))]
            [vox-sample-list (list-comp (os.path.join vox-sample-folder f) [f (os.listdir vox-sample-folder)])]
            
            [samples-drums (sum (list-comp [(itf.smp_add (Sample_File :name (% "bassdrum-evolved-%d" x) :filename (sfxr-genetics "./sfxr-drums/bassdrum" (% "bassdrum-%d" x))))
                                            (itf.smp_add (Sample_File :name (% "snaredrum-evolved-%d" x) :filename (sfxr-genetics "./sfxr-drums/snare" (% "snaredrum-%d" x))))] [x (range 8)]) [])]
            [sample-break (random.choice ["amen.wav" "think.wav"])]
            [break-chunk-count 8]
            [sample-chunks-break (list-comp (itf.smp_add (Sample_FileSlice :filename (os.path.join samples sample-break) :slices break-chunk-count :which s)) [s (range break-chunk-count)])]
            [length-break-chunk (len (getattr (get itf.smplist (- (get sample-chunks-break 0) 1)) "data"))]
            [length-beat (int (* 44100 (/ 60.0 itf.tempo)))]
            [break-note (ftom (* (mtof 60) (/ length-break-chunk length-beat) (/ break-chunk-count 4)))]
            [samples-weirdos (get-samples-weirdos itf 9)]
            [melody-sparseness (list-comp (+ (* (random.random) 0.9) 0.1) [x (range 3)])]
            ; compute a two basic sequences of notes using the fractal melody method
            [sequences (list-comp (make-fractal-note-sequence (random.choice [16 32]) 4 :sparseness-probability (get melody-sparseness x)) [x (range 3)])]
            [octave-sequences (list-comp (make-octave-sequence (random.choice [16 32]) (* (random.random) 0.25)) [x (range 3)])]
            ; bass variation
            [sequence-bass (slice (list (reversed (get sequences 0))) 0 16)]
            ; select some notes randomly to map to the sequence
            [root-note (random.randint 54 (+ 54 12 -1))]
            [notes-set (get-good-notes 4)]
            [notes-sets [notes-set (transform-notes-flip notes-set) (transform-notes-multiply notes-set)]]
            [[sample-melody sample-melody-pitch] (choose-bleep-sample itf "hi")]
            [[sample-bass sample-bass-pitch] (choose-bleep-sample itf "lo")]
            
            [melody-pattern (random.choice [[0 0 0 0 1 1 1 1 0 0 0 0 2 2 2 2]
                                            [0 0 1 1 0 0 1 1 2 2 2 2]])]
            [breaks-pattern [0 0 0 0 1 1 1 1 2 2 2 2 1 1 1 1]]
            [weirdos-pattern [0 0 0 0 1 1 1 1 2 2 2 2]]
            [vox-pattern (random.choice [[2 2 2 2 0 0 0 0 1 1 1 1]
                                         [0 0 1 1 0 0 2 2]
                                         [2 2 1 1 0 0 1 1 0 0 2 2]
                                         [1 1 1 1 0 0 0 0]])]
            
            [melody-fns-main (list-comp (make-melody-fn sample-melody (+ sample-melody-pitch root-note) (get sequences x) (get octave-sequences x) (get notes-sets x)
                                                        :pace 4
                                                        :volume 40
                                                        :octave-wander-probability (if (< (random.random) 0.333) (* (random.random) 0.75) 0)) [x (range 3)])]
            [melody-fns-bass (list-comp (make-melody-fn sample-bass (+ sample-bass-pitch root-note) (get [sequence-bass sequence-bass (get sequences 1)] x) (list-comp 0 [o (xrange 8)]) (get notes-sets x)
                                                        :pace 8
                                                        :volume 64
                                                        :octave-wander-probability (if (< (random.random) 0.333) (* (random.random) 0.25) 0)) [x (range 3)])]
            [breaks-fns (list-comp (make-breaks-fn sample-chunks-break (get-wrapped samples-drums (* x 2)) (get-wrapped  samples-drums (+ (* x 2) 1)) :break-pitch (int (math.floor break-note)) :seed (random.random)) [x (range 3)])]
            [weirdos-fns (list-comp (make-random-placement-fn (slice samples-weirdos (* x 3) (+ (* x 3) 3)) :volume 48 :seed (random.random) :number-per-pattern-segment 2 :probability 0.9) [x (range 3)])]
            [vox-fns (+ (list-comp (make-vox-fn (partial choose-vox-sample itf (random.sample vox-sample-list 30) {} (random.Random (* (random.random) (+ x 1))))) [x (range 2)])
                       [(make-pass-fn)])]
            
            [drop-groups (sorted [[1 5 6 7] [1 5 6] [1 4] [1 6] [0 1] [0 1 3] [0 5 6 7] [0 3 4]] :key (fn [x] (len x)))]
            [never-drop [2]]
            
            [master-key (if (< (random.random) 0.6) Key_Minor Key_Major)]
            
            [strategy (Strategy_Main root-note master-key 128 32)]]

        (print "Melody sparseness:" melody-sparseness)
        
        (print "Vox set:" vox-sample-folder)
        
        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-main melody-pattern)))
        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-bass melody-pattern)))
        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn weirdos-fns weirdos-pattern)))
        
        (strategy.gen_add (Generator_Callback 2 (make-section-lookup-fn breaks-fns breaks-pattern)))
        
        (let [[beat-seed (random.random)]
              [drum-kit (random.choice ["chip" "808k" "canb"])]]
          (print "Drum kit:" drum-kit)
          (defn drum-sample-chooser [samplename drum-kit]
            (cond
              [(= drum-kit "chip") (get samples-drums (get {"bass" 6 "snare" 7} samplename))]
              [(= drum-kit "808k") (itf.smp_add (Sample_File :name (+ "808-" samplename "drum") :filename (print-through "808 sample " (get-random-sample "808" samplename))))]
              [(= drum-kit "canb") (itf.smp_add (Sample_File :name (+ "CanOfBeats-" samplename "drum") :filename (print-through "CanOfBeats sample " (get-random-sample "CanOfBeats" (get {"bass" "bd" "snare" "sd"} samplename)))))]))
          (strategy.gen_add (Generator_Callback 1 (make-probability-table-fn (drum-sample-chooser "bass" drum-kit) :probability-table (-> beats (get :hiphop) (get :bd)) :seed beat-seed :trippy (< (random.random) 0.05))))
          (strategy.gen_add (Generator_Callback 1 (make-probability-table-fn (drum-sample-chooser "snare" drum-kit) :probability-table (-> beats (get :hiphop) (get :sd)) :seed beat-seed :trippy (< (random.random) 0.1)))))
        
        (let [[samples-808-hihat (list-comp (itf.smp_add (Sample_File :name (+ "808-hihat-" (unicode b)) :filename (get-random-sample "808" "hi hat-snappy"))) [b (xrange 3)])]]
          (strategy.gen_add (Generator_Callback 1 (make-hats-fn samples-808-hihat))))
        
        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn vox-fns vox-pattern)))
        
        ; add number of channels used to the itfile 'message' area
        (add-itf-message-line itf (+ "channels " (str (+ (max (list-comp (get g 0) [g strategy.gens])) 1))))
        (add-itf-message-line itf (+ "songdata-seed-int " (str (% (int seed-hash 16) (pow 2 23)))))
        (for [n (range (len notes-sets))]
          (add-itf-message-line itf (+ "songdata-notes-set-" (str n) " " (.join " " (list-comp (str x) [x (get notes-sets n)])))))
        (add-itf-message-line itf (+ "songdata-melody-pattern " (.join " " (list-comp (str m) [m melody-pattern]))))
        
        (for [i (xrange 28)]
          (print "pattern" i)
          (itf.ord_add (itf.pat_add (strategy.get_pattern))))
        
        (print "Applying post-fx")
        (let [[seeds (list-comp (random.random) [x (range 4)])]
              [pattern [0 0 0 1 2 2 2 3]]
              [drop-group-base-seed (random.random)]]
          (for [p (range (len itf.patlist))]
            (print "pattern" p)
            (apply-fx-to-pattern (get itf.patlist p) 3 :seed (get seeds (get-wrapped pattern p)))
            ; double whammy
            (apply-fx-to-pattern (get itf.patlist p) 3)
            ; also do 808 snares
            (apply-fx-to-pattern (get itf.patlist p) 6)
            ; drop-groups make things less or more busy
            (apply-drop-groups-to-pattern (get itf.patlist p) p drop-groups never-drop :seed drop-group-base-seed)))
        itf))))

(if (= __name__ "__main__")
  (main sys.argv))
