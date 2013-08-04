;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               load-libraries.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             MacOSX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads the CoreServices framework and interface files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")


(defparameter *additionnal-headers-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults *load-pathname*))


(defun headers-wild-pathname (name bits)
  (merge-pathnames (make-pathname
                    :case :local
                    :directory (list :relative
                                     (format nil "headers~A" bits)
                                     name
                                     :wild-inferiors)
                    :name :wild :type :wild
                    :defaults *additionnal-headers-directory*)
                   *additionnal-headers-directory*
                   nil))


(defun add-headers-logical-pathname-translations (name)
  (setf (logical-pathname-translations "CCL")
        (list* (list (make-pathname :host "CCL"
                                    :case :local
                                    :name :wild
                                    :type :wild
                                    :directory (list :absolute
                                                     "darwin-x86-headers"
                                                     name
                                                     :wild-inferiors))
                     (headers-wild-pathname name 32))
               (list (make-pathname :host "CCL"
                                    :case :local
                                    :name :wild
                                    :type :wild
                                    :directory (list :absolute
                                                     "darwin-x86-headers64"
                                                     name
                                                     :wild-inferiors))
                     (headers-wild-pathname name 64))
               (logical-pathname-translations "CCL"))))

;; (setf *additionnal-headers-directory* #P"~/works/patchwork/patchwork/src/pw-lib/pwscript/")


(defun load-coreservices-library ()
  (ccl:open-shared-library "/System/Library/Frameworks/CoreServices.framework/CoreServices")
  (ccl:use-interface-dir :coreservices))


(add-headers-logical-pathname-translations "coreservices")
(load-coreservices-library)
(pushnew :has-appleevent *features*)


#||

(cd #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/pwscript/")
(load #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/pwscript/appleevent-toolkit.lisp")

||#

;; See: headers32/coreservices/C/populate.sh
;; See: headers64/coreservices/C/populate.sh
;; /Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/CoreServices.framework/Headers/CoreServices.h
;; /Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/CoreServices.framework/Frameworks/AE.framework/Headers/

;;;; THE END ;;;;
