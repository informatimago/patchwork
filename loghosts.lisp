;;;; -*- mode:lisp;coding:us-ascii; -*-
(in-package "COMMON-LISP-USER")

(defvar *logical-hosts* '())

(defun make-translations (host logical-dir physical-dir &optional file-type)
  "
Returns logical pathname translations for the given HOST, mapping the
logical directory LOGICAL-DIR and all the files with the given
FILE-TYPE, in its subdirectories, to the physical directory
PHYSICAL-DIR.

If no FILE-TYPE is given, or if it's NIL, then a wildcard is used for
the file type, and the logical pathnames are translated with and
without this wildcard, in an orde that's implementation dependant.
The inclusion  of a version wildcard is also implementation dependant.
"
  (labels ((string-upper-case-p (s) (and (stringp s) (notany (function lower-case-p) s)))
           (string-lower-case-p (s) (and (stringp s) (notany (function upper-case-p) s)))
           (invert-case (list)
             (mapcar (lambda (item)
                       (cond
                         ((string-upper-case-p item)
                          (string-downcase item))
                         ((string-lower-case-p item)
                          (string-upcase item))
                         (t item)))
                     list)))
    (mapcan
     (lambda (item)
       (destructuring-bind (logical-tail physical-tail) item
         (let ((phys (format nil "~A**/~A" physical-dir physical-tail))
               (ild (invert-case logical-dir))
               (ilt (invert-case logical-tail)))
           (list*
            (list (apply (function make-pathname)
                         :host host
                         :directory `(:absolute ,@logical-dir :wild-inferiors)
                         :case :common
                         logical-tail)
                  phys)
            (unless (and (equal logical-dir ild)
                         (equal logical-tail ilt))
              (list
               (list (apply (function make-pathname)
                            :host host
                            :directory `(:absolute ,@ild :wild-inferiors)
                            :case :common
                            ilt)
                     phys)))))))
     #+clisp
     (if file-type
         `(((:name :wild :type ,file-type :version nil) ,(format nil "*.~(~A~)" file-type)))
         '(((:name :wild :type :wild      :version nil)   "*.*")
           ((:name :wild :type nil        :version nil)   "*")))
     #+sbcl
     (if file-type
         `(((:name :wild :type ,file-type :version :wild) ,(format nil "*.~(~A~)" file-type)))
         '(((:name :wild :type :wild :version :wild) "*.*")))
     #+(or allegro ccl)
     (if file-type
         `(((:name :wild :type ,file-type :version nil) ,(format nil "*.~(~A~)" file-type)))
         '(((:name :wild :type nil        :version nil) "*")
           ((:name :wild :type :wild      :version nil) "*.*")))
     #-(or clisp sbcl allegro ccl)
     (if file-type
         `(((:name :wild :type ,file-type :version nil)   ,(format nil "*.~(~A~)" file-type))
           ((:name :wild :type ,file-type :version :wild) ,(format nil "*.~(~A~)" file-type)))
         '(((:name :wild :type nil        :version nil)   "*")
           ((:name :wild :type :wild      :version nil)   "*.*")
           ((:name :wild :type :wild      :version :wild) "*.*"))))))


(defun set-logical-pathname-translations (host translations)
  "
Defines a new set of logical pathname translations (or overrides
existing ones) for the given HOST.

The HOST is added to the list of logical hosts defined.

TRANSLATIONS: a list of logical pathname translations.
"
  (print host) (map nil 'print translations)
  (pushnew host *logical-hosts* :test (function string-equal))
  (and (ignore-errors (setf (logical-pathname-translations host) nil) t)
       (setf (logical-pathname-translations host) translations)))


(let ((home #+(and)(user-homedir-pathname) #-(and)#P"/home/pjb/"))
  (let ((pw (make-pathname :directory (butlast (pathname-directory *load-truename*))
                           :name nil :type nil :type nil :defaults *load-truename*)))

    (set-logical-pathname-translations
     "SRC"
     (append
      (make-translations "SRC"       '("INFORMATIMAGO") (merge-pathnames "src/public/lisp/"  home))
      (make-translations "SRC"       '("MCLGUI")        (merge-pathnames "mclgui/"           pw))
      (make-translations "SRC"       '("PATCHWORK")     (merge-pathnames "patchwork/"        pw))
      (make-translations "SRC"       '()                (merge-pathnames "src/"              home))))

    (set-logical-pathname-translations
     "PATCHWORK"
     (append
      (make-translations "PATCHWORK" '("MCLGUI")        (merge-pathnames "mclgui/"           pw))
      (make-translations "PATCHWORK" '()                (merge-pathnames "patchwork/"        pw))))

    (set-logical-pathname-translations
     "CLENI"
     (make-translations "CLENI"      '()                (merge-pathnames "patchwork/src/pw-lib/cleni/" pw)))


    (set-logical-pathname-translations
     "PW-USER"
     (make-translations "PW-USER"    '()                (merge-pathnames "Documents/Patchwork/"        home)))

    ))
