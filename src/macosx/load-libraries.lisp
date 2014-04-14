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

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun load-coreservices-library ()
    (add-headers-logical-pathname-translations "coreservices")
    (ccl:open-shared-library "/System/Library/Frameworks/CoreServices.framework/CoreServices")
    (ccl:use-interface-dir :coreservices)
    (pushnew :has-appleevent *features*))

  (defun load-midishare-library ()
    (add-headers-logical-pathname-translations "midishare")
    (ccl:open-shared-library "/Library/Frameworks/MidiShare.framework/MidiShare")
    (ccl:use-interface-dir :midishare)
    (pushnew :has-midishare *features*))

  );;eval-when


(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-coreservices-library)
  (load-midishare-library))


#||

(cd #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/pwscript/")
(load #P"/home/pjb/works/patchwork/patchwork/src/pw-lib/pwscript/appleevent-toolkit.lisp")

||#

;; See: headers32/coreservices/C/populate.sh
;; See: headers64/coreservices/C/populate.sh
;; /Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/CoreServices.framework/Headers/CoreServices.h
;; /Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/CoreServices.framework/Frameworks/AE.framework/Headers/

;;;; THE END ;;;;
