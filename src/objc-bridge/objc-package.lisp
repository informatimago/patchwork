;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages of the objc-bridge.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-02 <PJB> Forked.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright (C) 2007-2009 Clozure Associates and contributors.
;;;;    Parts of this file were part of Clozure CL.  
;;;;
;;;;    Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;;    License , known as the LLGPL and distributed with Clozure CL as the
;;;;    file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;;    which is distributed with Clozure CL as the file "LGPL".  Where these
;;;;    conflict, the preamble takes precedence.  
;;;;
;;;;    Clozure CL is referenced in the preamble as the "LIBRARY."
;;;;
;;;;    The LLGPL is also available online at
;;;;    http://opensource.franz.com/preamble.html
;;;;
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(in-package "CL-USER")


(defpackage "NS"
  (:use)
  (:export "+CGFLOAT-ZERO+" "CGFLOAT" "CG-FLOAT")
  (:documentation "All class names and instance variable names are interned in the NS package."))


(defpackage "NEXTSTEP-FUNCTIONS"
  (:use)
  (:nicknames "NSFUN")
  (:documentation "ObjC function names (as produced by #/) are interned in NEXTSTEP-FUNCTIONS package."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  #-ccl (warn "Export stuff from NS and NSFUN")
  ;; Force all symbols interned in the NS package to be external
  #+ccl (ccl::package-force-export "NS")
  #+ccl (ccl::package-force-export "NEXTSTEP-FUNCTIONS"))


(defpackage "OBJC"
  (:use "COMMON-LISP")
  (:shadow "DEFMETHOD")
  (:export "OBJC-OBJECT" "OBJC-CLASS-OBJECT" "OBJC-CLASS" "OBJC-METACLASS"
           "@CLASS" "@SELECTOR" "MAKE-OBJC-INSTANCE" "RETURNING-FOREIGN-STRUCT"
           "DEFMETHOD" "SLET" "SEND" "SEND/STRET" "SEND-SUPER" "SEND-SUPER/STRET"
           "DEFINE-OBJC-METHOD" "DEFINE-OBJC-CLASS-METHOD"
           "OBJC-MESSAGE-SEND" "OBJC-MESSAGE-SEND-STRET"
           "OBJC-MESSAGE-SEND-SUPER" "OBJC-MESSAGE-SEND-SUPER-STRET"
           "LOAD-FRAMEWORK" "*OBJC-DESCRIPTION-MAX-LENGTH*"
           "REMOVE-LISP-SLOTS" "WITH-AUTORELEASE-POOL"
           "MAKE-NSSTRING" "LISP-STRING-FROM-NSSTRING"
           "WITH-AUTORELEASED-NSSTRINGS"))



(defpackage "OBJC-BRIDGE.SEQUENCE-UTILS"
  (:use "COMMON-LISP")
  (:export "SPLIT-IF" "SPLIT-IF-CHAR" "SPLIT-LINES"
           "MATCH-SUBSEQUENCE" "FIND-MATCHING-SUBSEQUENCE")
  (:documentation "Exports a few sequence utility functions."))




(defpackage "OBJC-BRIDGE"
  (:use "COMMON-LISP"
        "OBJC-BRIDGE.SEQUENCE-UTILS"
        "OBJC")
  (:shadowing-import-from "COMMON-LISP" "DEFMETHOD")
  (:export
   ;; reader macros
   "LEFT-BRACKET-READER-MACRO"
   "SHARP-SLASH-READER-MACRO"
   "SHARP-AT-READER-MACRO"
   "SET-OBJC-READER-MACROS"
   "RESET-OBJC-READER-MACROS"
   "ENABLE-OBJC-READER-MACROS"
   "DISABLE-OBJC-READER-MACROS"
   ;;
   "INITIALIZE"

   )
  (:documentation "

This is a fork of the Objective-C bridge from ccl-1.8, whose purpose
is to be modular (ie. loadable independently from ccl, with
quicklisp/asdf), and eventually portable (using CFFI, CLOSER-MOP, etc),
while still targetting apple, cocotron and gnustep Objective-C.

"))


;;;; THE END ;;;;
