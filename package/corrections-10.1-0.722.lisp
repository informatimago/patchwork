;;;;
;;;; Corrections sur la version 10.1-0.722
;;;;
;;;; Fichier à placer dans ~/Documents/Patchwork/PW-init/
;;;;
;;;;

;;;;--------------------------------------------------------------------------
(in-package "PATCHWORK.BUILDER")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")

(defun version (major minor &rest rest)
  (list* major minor rest))

(defun version-split (version-string)
  "We handle n{.n}[-n{.n}]"
  (let ((dash (position #\- version-string)))
    (if dash
        (append (version-split (subseq version-string 0 dash))
                (mapcar (function -)
                        (version-split (subseq version-string (1+ dash)))))
        (mapcar (function parse-integer)
                (split-string version-string ".")))))

(defun version-join (version)
  (let ((neg (position-if (function minusp) version)))
    (if neg
        (format nil "~{~A~^.~A~}-~{~A~^.~A~}"
                (subseq version 0 (1- neg))
                (mapcar (function -) (subseq version (1- neg))))
        (format nil "~{~A~^.~A~}" version))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapcar (function fmakunbound)
          '(version= version< version<=)))

(defgeneric version= (a b)
  (:method ((a string) b)      (version= (version-split a) b))
  (:method (a (b string))      (version= a (version-split b)))
  (:method ((a null) b)        (every (function zerop) b))
  (:method (a (b null))        (every (function zerop) a))
  (:method (a b)               (and (equal (first a) (first b))
                                    (version= (rest a) (rest b)))))

(defgeneric version< (a b)
  (:method ((a string) b)      (version< (version-split a) b))
  (:method (a (b string))      (version< a (version-split b)))
  (:method ((a null) (b null)) t)

  (:method ((a null) b)
    (loop :while b
          :do (let ((item (pop b)))
                (cond
                  ((plusp  item) (return t))
                  ((minusp item) (return nil))))
          :finally (return t)))

  (:method (a (b null))
    (loop :while a
          :do (let ((item (pop a)))
                (cond
                  ((plusp  item) (return nil))
                  ((minusp item) (return t))))
          :finally (return nil)))

  (:method (a b)
    (cond
      ((< (car a) (car b)) t)
      ((= (car a) (car b)) (version< (cdr a) (cdr b)))
      (t                   nil))))

(defgeneric version<= (a b)
  (:method (a b)
    (or (version= a b) (version< a b))))


;;;;--------------------------------------------------------------------------
(in-package "PW")

#+(:and
   #.(patchwork.builder:rt-version<=  "10.1-0.722" patchwork.builder:*patchwork-version*)
   #.(patchwork.builder:rt-version<=  patchwork.builder:*patchwork-version* "10.1-0.700"))
(defun evaluate-patch (patch)
  ;; Note: we format a string before writing it in whole to get around a bug in the MAKE-LISTENER-IO code.
  (handler-case (patch-value patch patch)
    (:no-error (&rest values)
      (setf *values* values
            *value*  (first values))
      (write-string (format nil "~&PW->~S~%" *value*))
      (multiple-value-list *values*))
    (error (err)
      (setf *values* (list patch err)
            *value*  patch)
      (let ((errmsg (handler-case (format nil "~A" err)
                      (error (err)
                        (format nil "Error while formating error message: ~A" err)))))
        (write-string (format nil "~&Error while evaluating patch PW->~S~%~A~%"
                              *value* errmsg))))))

;;;;--------------------------------------------------------------------------
(in-package "CL-USER")
#+(:and
   #.(patchwork.builder:rt-version<=  "10.1-0.722" patchwork.builder:*patchwork-version*)
   #.(patchwork.builder:rt-version<=  patchwork.builder:*patchwork-version* "10.1-0.700"))
(format t "Patched ~S~%" 'evaluate-patch)

;;;; THE END ;;;;
