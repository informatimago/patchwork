;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-bridge.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    This is a fork of ccl objc-bridge.  Eventually it would work
;;;;    over CFFI on any implementation..
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-02 <PJB> Created.
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

(asdf:defsystem :objc-bridge
    :name "objc-bridge"
    :description "Objective-C Bridge"
    :author "Randall D. Beer & Pascal J. Bourguignon"
    :version "1.0.1"
    :license "LLGPL"
    :depends-on ("closer-mop" "bordeaux-threads" "cffi") 
    :components ((:file "objc-package")

                 (:file "sequence-utils"
                        :depends-on ("objc-package"))

                 (:file "macros"
                        :depends-on ("objc-package"))

                 (:file "name-translation"
                        :depends-on ("objc-package"
                                     "sequence-utils"))

                 (:file "objc-readtable"
                        :depends-on ("objc-package"
                                     "name-translation"))
                 
                 (:file "objc-clos"
                        :depends-on ("objc-package"
                                     "name-translation"
                                     "sequence-utils"))

                 (:file "objc-classes"
                        :depends-on ("objc-package"
                                     "objc-readtable"))

                 
                 (:file "library"
                        :depends-on ("objc-package"
                                     "objc-classes"))
                 
                 (:file "objc-runtime"
                        :depends-on ("objc-package"
                                     "name-translation"
                                     "macros"
                                     "objc-clos"
                                     "objc-readtable"
                                     "library"))
                 
                 (:file "initialize"
                        :depends-on ("objc-package"
                                     "name-translation"
                                     "sequence-utils"
                                     "macros"
                                     "library"
                                     "objc-runtime"))
                 
                 (:file "bridge"
                        :depends-on ("objc-package"
                                     "macros"
                                     "name-translation"
                                     "objc-clos"
                                     "objc-classes"
                                     "objc-runtime"
                                     "library"
                                     "initialize"))

                 (:file "objc-support"
                        :depends-on ("objc-package"
                                     "macros"
                                     "objc-readtable"
                                     "library"
                                     "bridge"
                                     "initialize"))

                 (:file "print-object"
                        :depends-on ("objc-package"
                                     "objc-runtime"
                                     "bridge"
                                     "objc-support"))))


;;;; THE END ;;;;
