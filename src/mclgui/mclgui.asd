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
    :version "1.0.3"
    :license "GPL3"
    :depends-on ("closer-mop" ; window.lisp uses closer-mop:class-precedence-list, etc.
                 "cffi" 
                 "trivial-gray-streams"
                 "alexandria"
                 "com.informatimago.objcl"
                 ;; While developping:
                 "com.informatimago.common-lisp.cesarum") 
    :components ((:file "package")

                 (:file "macros"
                        :depends-on ("package"))

                 (:file "variables"
                        :depends-on ("package" 
                                     "point"))
                 (:file "system"
                        :depends-on ("package"))
                 
                 (:file "wrapper"
                        :depends-on ("package"))

                 (:file "notification"
                        :depends-on ("package"
                                     "wrapper"))

                 (:file "objc-classes"
                        :depends-on ("package"
                                      "macros" "wrapper" "variables" "point"))
                 (:file "graphics"
                        :depends-on ("package"
                                     "macros" "variables" "point"))
                 
                 ;; Chapter 2:

                 (:file "point"
                        :depends-on ("package"))

                 (:file "font"
                        :depends-on ("package" 
                                     "macros" "variables" "system"
                                     "point" "objc-classes"))
                 
                 (:file "pattern"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "wrapper"))

                 ;; Chapter 3: Menus

                 (:file "menu"
                        :depends-on ("package" 
                                     "macros" "variables" "color"
                                     "point" "font" "wrapper" "notification"))

                 (:file "window-menu-item"
                        :depends-on ("package" 
                                     "menu" "window"))
                 
                 ;; Chapter 4: Views and Windows

                 (:file "view-classes"
                        :depends-on ("package" 
                                     "macros" "variables" "color"
                                     "wrapper"))

                 (:file "view"
                        :depends-on ("package" 
                                     "macros" "variables" "color"
                                     "point" "region" "font" "pen"
                                     "wrapper" "view-classes" "objc-classes"))

                 (:file "view-stream"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "font" "pen"
                                     "view-classes" "view" "graphics"))

                 (:file "window"
                        :depends-on ("package" 
                                     "macros" "variables" "color"
                                     "point" "region" "font"
                                     "view-classes" "objc-classes"
                                     "wrapper" "view" "notification"))

                 (:file "fred-window"
                        :depends-on ("package" 
                                     "macros" "variables" "color"
                                     "point" "region" "font"
                                     "view-classes" "view" "window"))

                 ;; Chapter 5: Dialog Items and Dialogs

                
                 (:file "dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "region"))

                 (:file "default-button-mixin"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"))

                 (:file "control-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item"))
                 
                 (:file "button-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "control-dialog-item"
                                     "default-button-mixin"))

                 (:file "check-box-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "control-dialog-item"
                                     "button-dialog-item"))

                 (:file "radio-button-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "control-dialog-item"
                                     "button-dialog-item"))
                 
                 (:file "static-text-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item"))

                 (:file "focus-rect-mixin"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item"))

                 (:file "key-handler-mixin"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system"
                                     "view-classes" "view" "window"))
                 
                 (:file "editable-text-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "view-event" "key-handler-mixin"))

                 (:file "table-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system" "region"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "view-event" "key-handler-mixin"
                                     "scroll-bar-dialog-item"))

                 (:file "sequence-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "view-event" "key-handler-mixin"
                                     "scroll-bar-dialog-item" "table-dialog-item"))

                 (:file "scroll-bar-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "view-event"))

                 (:file "pop-up-menu-dialog-item"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "view-event"))

                 (:file "dialog"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "key-handler-mixin"))

                 (:file "y-or-n-dialog"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "key-handler-mixin"
                                     "button-dialog-item" 
                                     "static-text-dialog-item" "editable-text-dialog-item"
                                     "dialog"))

                 (:file "select-dialog"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "key-handler-mixin"
                                     "button-dialog-item" 
                                     "static-text-dialog-item" "editable-text-dialog-item"
                                     "dialog"))

                 (:file "message-dialog"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "key-handler-mixin"
                                     "button-dialog-item" 
                                     "static-text-dialog-item" "editable-text-dialog-item"
                                     "dialog"))

                 (:file "get-string-dialog"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "key-handler-mixin"
                                     "button-dialog-item" 
                                     "static-text-dialog-item" "editable-text-dialog-item"
                                     "dialog"))

                 (:file "scroller"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system" "graphics"
                                     "view-classes" "view" "window" "event"
                                     "dialog-item" "key-handler-mixin"
                                     "scroll-bar-dialog-item"
                                     "dialog"))
                 
                 ;; Chapter 6: Color

                 (:file "color"
                        :depends-on ("package"))

                 ;; Chapter 8: File System Interface

                 (:file "file"
                        :depends-on ("package"))
                 
                 ;; We only implement the file chooser dialogs.
                 ;; The rest is covered by CL.
                 

                 ;; Chapter 10: Events

                 (:file "event"
                        :depends-on ("package" 
                                     "macros" "variables" "point" "system"
                                     "objc-classes"
                                     "view-classes" "view" "window"))
                 
                 (:file "view-event"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system" "view-classes" "view"
                                     "event" "key-handler-mixin"))

                 (:file "window-event"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "system" "view-classes" "window"
                                     "view-event" "event" "application"))

                 
                 (:file "cursor"
                        :depends-on ("package" 
                                     "macros" "variables" "point"
                                     "wrapper" "pattern"))
                 
                 (:file "scrap"
                        :depends-on ("package" 
                                     "macros" "variables" "point"))
                 
                 (:file "eval"
                        :depends-on ("package" 
                                     "macros" "variables" "application"))

                 ;; Chapter 11: Apple Events

                 (:file "application"
                        :depends-on ("package" 
                                     "macros" "variables" "wrapper"
                                     "objc-classes"))
                 
                 ;; Appendix D: Quickdraw Graphics:

                 (:file "rect"
                        :depends-on ("package" "point"))

                 (:file "region"
                        :depends-on ("package" "macros" "variables" "point"))

                 (:file "pen"
                        :depends-on ("package"
                                     "macros" "variables" "point" 
                                     "objc-classes" "view-classes"
                                     "pattern"
                                     "rect"))


                 ;; MCLGUI:

                 (:file "mclgui"
                        :depends-on ("package"
                                     "macros" "variables"
                                     "point" "font" "pen" "pattern" "cursor" "view-stream"
                                     "menu" "view" "window" "dialog" "fred-window"
                                     "dialog-item" "control-dialog-item"
                                     "button-dialog-item" "check-box-dialog-item"
                                     "radio-button-dialog-item" "static-text-dialog-item"
                                     "focus-rect-mixin" "editable-text-dialog-item"
                                     "table-dialog-item" "scroll-bar-dialog-item"
                                     "pop-up-menu-dialog-item" "sequence-dialog-item"
                                     "y-or-n-dialog" "message-dialog" "get-string-dialog"
                                     "select-dialog"
                                     "scroller"
                                     "event" "view-event" "window-event"
                                     "cursor" "scrap" "eval" "application"))))


;;;; THE END ;;;;
