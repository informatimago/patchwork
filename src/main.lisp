(in-package "COMMON-LISP-USER")

(defun start-patchwork ()
  (ui:on-main-thread/sync
    (handler-case
        (progn
          (unless (typep ui:*application* 'pw::patchwork-application)
            (ui:format-trace 'start-patchwork "change-class application")
            (change-class ui:*application* 'pw::patchwork-application))
          (ui:format-trace 'start-patchwork 'ui:initialize)
          (ui:initialize)
          #+(or patchwork-cocoa-midi-player
                patchwork-use-cl-midi
                patchwork-use-midishare)
          (patchwork.midi:initialize-midi)
          (pw::initialize-patchwork ui:*application*)
          (pw::show-welcome ui:*application*))
      (error (err)
        (format *error-output* "~A~%" err)))))

