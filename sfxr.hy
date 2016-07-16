(import os)
(import json)
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

