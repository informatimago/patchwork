;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               import.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Extracted import from epw files to here for file dependencies.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Proprietary
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    All Rights Reserved.
;;;;    
;;;;    This program and its documentation constitute intellectual property 
;;;;    of Pascal J. Bourguignon and is protected by the copyright laws of 
;;;;    the European Union and other countries.
;;;;**************************************************************************

(in-package "EPW")

(import '(pw::new-menu pw::PW-addmenu))

;;(import '(mc->f1 f->mc1)) ; etc.
;;(import '(unique-sorted f->mc1 mc->f1 f->mc mc->f cents->coef coef->cents))
