;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    Defines the MCLGUI asdf system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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

(asdf:defsystem :mclgui
    :name "mclgui"
    :description "Macintosh Common Lisp Graphical User Interface for OpenStep"
    :author "Pascal J. Bourguignon"
    :version "1.0.0"
    :license "GPL3"
    :depends-on ()
    :components ((:file "package")
                 (:file "macros"           :depends-on ("package"))
                 (:file "variables"        :depends-on ("package"))
                 ;; Chapter 2:
                 (:file "point"            :depends-on ("package"))
                 (:file "font"             :depends-on ("package" "macros" "variables" "point" ))
                 ;; Chapter 3: Menus
                 (:file "menu"             :depends-on ("package" "macros" "variables" "point" "font"))
                 (:file "window-menu-item" :depends-on ("package" "menu" "window"))
                 ;; Chapter 4: Views and Windows
                 
                 ;; (:file "window"           :depends-on ("package" "macros" "variables" "point" "font"))
                 ;; Chapter 5: Dialog Items and Dialogs
                 ;; Chapter 6: Color
                 ;; Chapter 10: Events
                 ;; (:file "event"            :depends-on ("package" "macros" "variables" "point"))
                 ;; Chapter 11: Apple Events
                 
                 (:file "mclgui"           :depends-on ("package"
                                                       "macros" "variables"
                                                       "point" "font" "menu"
                                                       ;; "window"
                                                       ;; "event"
                                                       ))))

;;;; THE END ;;;;
