;;;;
;;;; init.lisp for Patchwork.
;;;;

(in-package "CL-USER")
(format t "~%Hello from ~A !~2%" *load-pathname*)

;; Define an alias for pw:*value*, since it's displayed as PW->value
(define-symbol-macro pw pw:*value*)


(defun patchwork () (eval-enqueue '(pw::pw-menu-action))   (values))
(defun lisp      () (eval-enqueue '(pw::lisp-menu-action)) (values))


;;;; THE END ;;;;
