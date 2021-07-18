;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loghosts.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the compilation-time logical hosts for this project.
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
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-07-18 <PJB> Split out compilation-time loghosts.
;;;;    2016-11-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2016 - 2021
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
(in-package "COMMON-LISP-USER")

(defun define-compilation-time-loghosts ()
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
    (let* ((home  (truename (user-homedir-pathname)))
           (src   (truename (merge-pathnames "../"
                                             (make-pathname :name nil
                                                            :type nil
                                                            :version nil
                                                            :defaults #.(or *compile-file-truename*
                                                                            *load-truename*
                                                                            #P"./")))))
           (home-src (merge-pathnames "src/" home)))
      (set-host "SRC"

                ;; '("INFORMATIMAGO")  "informatimago/"                         src
                '("INFORMATIMAGO")  "public/lisp/"                           home-src

                '("PATCHWORK")      "patchwork/"                             src
                '("MCLGUI")         "mclgui/"                                src
                '("MIDI")           "midi/"                                  src
                '("MIDISHARE")      "midishare/"                             src
                '("CLENI")          "patchwork/src/src-lib/cleni/"           src
                '()                 "src/"                                   home)

      (set-host "PATCHWORK"
                '()                 "patchwork/"                             src)

      (set-host "MCLGUI"
                '()                 "mclgui/"                                src)

      (set-host "COREMIDI"
                '()                 "CoreMIDI/"                              src)

      (set-host "MIDI"
                '()                 "midi/"                                  src)))

  *logical-hosts*)

(define-compilation-time-loghosts)


#+ccl
(defun update-interfaces-logical-pathname-translations ()
  (setf (logical-pathname-translations "ccl")
        (append
         (let ((ccl-sources (make-pathname :directory (butlast
                                                       (pathname-directory
                                                        (translate-logical-pathname #P"ccl:compiler;")))
                                           :defaults #P"/")))
           (loop
             :for dir :in (append (directory (translate-logical-pathname "SRC:PATCHWORK;SRC;MACOSX;HEADERS64;*;"))
                                  (directory (merge-pathnames #P"darwin-x86-headers64/*/" ccl-sources)))
             :for name := (first (last (pathname-directory dir)))
             :collect (list (format nil "ccl:darwin-x86-headers64;~A;**;*.*" name)
                            (merge-pathnames #P";**;*.*" dir))))
         (logical-pathname-translations "ccl"))))

;; Update the ccl:darwin-x86-headers64; logical-pathname-translations, for compilation-time.
#+ccl (update-interfaces-logical-pathname-translations)


(setf (logical-pathname-translations "PATCHWORK")
      (append (make-translations "PATCHWORK"
                                 '("SRC" "MACOSX" "RESOURCES" "FONTS")
                                 (merge-pathnames #P"src/macosx/Resources/Fonts/"
                                                  (translate-logical-pathname #P"PATCHWORK:")
                                                  nil))
              (logical-pathname-translations "PATCHWORK")))

;;;; THE END ;;;;
