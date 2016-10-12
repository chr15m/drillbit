(import os)
(import json)
(import math)
(import random)
(import [glob [glob]])
(import [subprocess [Popen PIPE]])
(import [hashlib [sha1]])

(import [chipvolver [load_definitions reproduce]])

(defn sfxr-render [definition filename]
  (.communicate (Popen ["./jsfxr/sfxr-to-wav" filename] :stdout PIPE :stdin PIPE) (json.dumps definition))
  filename)

(defn sfxr-genetics [startswith name]
  (let [[seed (.hexdigest (sha1 (str (random.random))))]]
    (print "sfxr genetics:" seed startswith name)
    (let [[wav-file-name (+ "samples/" name "-" (str seed) "-evolved.wav")]
          [already-generated (os.path.isfile wav-file-name)]]
      (if (not already-generated)
        (let [[[sample-evolved-definition seed-used] (reproduce (load_definitions (glob (+ startswith "*.sfxr.json"))) :seed seed)]]
          (sfxr-render sample-evolved-definition wav-file-name))
        (print "already-generated"))
      wav-file-name)))

(def keys ["oldParams"
           "wave_type"
           "p_env_attack"
           "p_env_sustain"
           "p_env_punch"
           "p_env_decay"
           "p_base_freq"
           "p_freq_limit"
           "p_freq_ramp"
           "p_freq_dramp"
           "p_vib_strength"
           "p_vib_speed"
           "p_arp_mod"
           "p_arp_speed"
           "p_duty"
           "p_duty_ramp"
           "p_repeat_speed"
           "p_pha_offset"
           "p_pha_ramp"
           "p_lpf_freq"
           "p_lpf_ramp"
           "p_lpf_resonance"
           "p_hpf_freq"
           "p_hpf_ramp"
           "sound_vol"
           "sample_rate"
           "sample_size"])

(defn make-bleep [&optional [freq-range "lo"] [r (random.Random)]]
  (let [[arp-mod (r.choice [0 0 0 0 -0.3162 0.7454 0.7454])]
        [pattern
         {"oldParams" true
          "wave_type" (r.choice [0 1])
          "p_env_attack" (if (r.choice [0 0 0 0 1]) (* 0.5 (r.random)) 0)
          "p_env_sustain" (r.random)
          "p_env_punch" (r.random)
          "p_env_decay" (+ (* (r.random) 0.9) 0.1)
          "p_base_freq" (if (or (= freq-range "hi") (< arp-mod 0)) 0.1737 0.12079)
          "p_freq_limit" 0
          "p_freq_ramp" 0
          "p_freq_dramp" 0
          "p_vib_strength" 0
          "p_vib_speed" 0
          "p_arp_mod" arp-mod
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
    pattern))

