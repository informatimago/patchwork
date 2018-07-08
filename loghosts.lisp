;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loghosts.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the logical hosts for this project.
;;;;
;;;;    We define the logical host used at compilation time in this file
;;;;    so that we may define them in function of the source directories.
;;;;
;;;;
;;;;    Logical Hosts used at compilation time
;;;;    --------------------------------------
;;;;
;;;;      PATCHWORK
;;;;
;;;;        The logical host PATCHWORK should be set so that the .git/
;;;;        subdirectory should be  at its root:
;;;;
;;;;            #+ccl (probe-file #P"PATCHWORK:.git;") --> true
;;;;
;;;;      MCLGUI
;;;;
;;;;      MIDI
;;;;
;;;;    Logical Hosts used at run-time
;;;;    ------------------------------
;;;;
;;;;    Those logical hosts are used by patchwork or its dependencies include:
;;;;
;;;;      PW-USER  -- See src/application.lisp  initialize-directories
;;;;
;;;;      CLENI
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-11-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2016 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(defpackage "PATCHWORK.LOGICAL-HOSTS"
  (:use "COMMON-LISP")
  (:shadow "MAKE-PATHNAME"
           "USER-HOMEDIR-PATHNAME"
           "TRANSLATE-LOGICAL-PATHNAME")
  (:export "*LOGICAL-HOSTS*"
           "MAKE-TRANSLATIONS"))
(in-package "PATCHWORK.LOGICAL-HOSTS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From com.informatimago.tools.pathname

(defparameter *case-common-is-not-downcased-on-posix-systems*
  #+(or allegro ccl emacs-cl) t
  #-(or allegro ccl emacs-cl) nil)


(defun user-homedir-pathname ()
  "On CCL on MS-Windows, it's not the USER-HOMEDIR-PATHNAME."
  #+(and ccl windows-target)
  (let ((home (ccl::getenv "HOME")))
    (if home
        (pathname (format nil "~A\\" home))
        ;; Fucking MoCL can't handle #+#P!!!
        (pathname "C:\\cygwin\\home\\pjb\\")))
  #-(and ccl windows-target)
  (cl:user-homedir-pathname))


(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
                        (name nil namep) (type nil typep) (version nil versionp)
                        (defaults nil defaultsp) (case :local casep))
  (declare (ignorable casep))
  (if *case-common-is-not-downcased-on-posix-systems*
      (labels ((localize (object)
                 (typecase object
                   (list   (mapcar (function localize) object))
                   (string (string-downcase object))
                   (t      object)))
               (parameter (indicator key value)
                 (when indicator
                   (list key (if (eql case :common)
                                 (localize value)
                                 value)))))
        (apply (function cl:make-pathname)
               (append (parameter hostp      :host      host)
                       (parameter devicep    :device    device)
                       (parameter directoryp :directory directory)
                       (parameter namep      :name      name)
                       (parameter typep      :type      type)
                       (parameter versionp   :version   version)
                       (parameter defaultsp  :defaults  defaults)
                       (list :case :local))))
      (apply (function cl:make-pathname)
             (append
              (when hostp      (list :host      host))
              (when devicep    (list :device    device))
              (when directoryp (list :directory directory))
              (when namep      (list :name      name))
              (when typep      (list :type      type))
              (when versionp   (list :version   version))
              (when defaultsp  (list :defaults  defaults))
              (when casep      (list :case      case))))))


(defun translate-logical-pathname (pathname)
  (cl:translate-logical-pathname
   (etypecase pathname
     (string             (translate-logical-pathname (pathname pathname)))
     #-mocl ; !!!!
     (logical-pathname   (make-pathname :host      (pathname-host pathname)
                                        :device    (pathname-device pathname)
                                        :directory (pathname-directory pathname)
                                        :name      (pathname-name pathname)
                                        :type      (pathname-type pathname)
                                        :version   (pathname-version pathname)
                                        :defaults  pathname
                                        :case      :common))
     (pathname           pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (labels ((string-upper-case-p (s)
             (and (stringp s) (notany (function lower-case-p) s)))
           (string-lower-case-p (s)
             (and (stringp s) (notany (function upper-case-p) s)))
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
                         :case :local
                         logical-tail)
                  phys)
            (unless (and (equal logical-dir ild)
                         (equal logical-tail ilt))
              (list
               (list (apply (function make-pathname)
                            :host host
                            :directory `(:absolute ,@ild :wild-inferiors)
                            :case :local
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
  (pushnew host *logical-hosts* :test (function string-equal))
  (and (ignore-errors (setf (logical-pathname-translations host) nil) t)
       (setf (logical-pathname-translations host) translations)))



(defun define-logical-hosts ()
  (flet ((set-host (host logical-subdir physical-subdir physical-dir
                    &rest other-definitions)
           (set-logical-pathname-translations
            host
            (loop
              :for (logical-subdir physical-subdir physical-dir)
                :on (list* logical-subdir physical-subdir physical-dir
                           other-definitions) :by (function cdddr)
              :append (make-translations host logical-subdir
                                         (merge-pathnames physical-subdir
                                                          physical-dir))))))
    (let* ((home  #+(and) (user-homedir-pathname)
                  #-(and) #P"/home/pjb/")
           (src   (merge-pathnames "../"
                                   (make-pathname :name nil
                                                  :type nil
                                                  :version nil
                                                  :defaults #.(or *compile-file-truename*
                                                                  *load-truename*
                                                                  #P"./"))))
           (home-src (merge-pathnames "src/" home)))
      (set-host "SRC"

                ;; '("INFORMATIMAGO")  "informatimago/"               src
                '("INFORMATIMAGO")  "public/lisp/"                 home-src

                '("PATCHWORK")      "patchwork/"                   src
                '("MCLGUI")         "mclgui/"                      src
                '("MIDI")           "midi/"                        src
                '("MIDISHARE")      "midishare/"                   src
                '()                 "src/"                         home)

      (set-host "PATCHWORK"
                '()                 "patchwork/"                   src)

      (set-host "MCLGUI"
                '()                 "mclgui/"                      src)

      (set-host "COREMIDI"
                '()                 "CoreMIDI/"                    src)

      (set-host "MIDI"
                '()                 "midi/"                        src)

      (set-host "CLENI"
                '()                 "patchwork/src/src-lib/cleni/" src)

      (set-host "PW-USER"
                '()                 "Documents/Patchwork/"         home)))
  *logical-hosts*)

(define-logical-hosts)

(setf (logical-pathname-translations "PATCHWORK")
      (append (make-translations "PATCHWORK"
                                 '("SRC" "MACOSX" "RESOURCES" "FONTS")
                                 (merge-pathnames #P"src/macosx/Resources/Fonts/"
                                                  (translate-logical-pathname #P"PATCHWORK:")
                                                  nil))
              (logical-pathname-translations "PATCHWORK")))

;;;; THE END ;;;;
