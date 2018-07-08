(in-package "PATCHWORK.MIDI")

(defun print-midi-devices ()
  (let ((*print-circle* nil))
    (flet ((endpoint-and-connected-device (endpoint)
             (list (coremidi:name endpoint)
                   (mapcar (function coremidi:name) (coremidi:connected-devices endpoint)))))
      (dolist (device (append (coremidi:devices)
                              (coremidi:external-devices)))
        (let ((entities      (coremidi:device-entities device)))
          (format t "~30A ~%"
                  (coremidi:name device))
          (dolist (entity entities)
            (format t "          - ~A~@[ <- ~{~S~^, ~}~]~@[ -> ~{~S~^, ~}~]~%"
                    (coremidi:name entity)
                    (mapcar (function endpoint-and-connected-device)
                            (coremidi:entity-sources entity))
                    (mapcar (function endpoint-and-connected-device)
                            (coremidi:entity-destinations entity))))
          (terpri))))))


(update-port-refnums)
(list (get-source-refnum '("SCHMIDT SYNTH" "SCHMIDT SYNTH" "SCHMIDT SYNTH"))
      (get-destination-refnum '("SCHMIDT SYNTH" "SCHMIDT SYNTH" "SCHMIDT SYNTH")))
(10065258428 10065258428)

(list (get-source-by-refnum (get-source-refnum '("SCHMIDT SYNTH" "SCHMIDT SYNTH" "SCHMIDT SYNTH")))
      (get-destination-by-refnum (get-destination-refnum '("SCHMIDT SYNTH" "SCHMIDT SYNTH" "SCHMIDT SYNTH")))
      (cm-input-port (MidiGetNamedAppl "PatchWork"))
      (cm-output-port (MidiGetNamedAppl "PatchWork")))
(#<endpoint :name "SCHMIDT SYNTH" :ref 262156341 #x302005A4566D> 
#<endpoint :name "SCHMIDT SYNTH" :ref 262156342 #x302005A455CD> 
#<port :name "PatchWork-IN" :ref 262156402 #x302004F122CD> 
#<port :name "PatchWork-OUT" :ref 262156401 #x302004F1262D>)
