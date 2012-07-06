;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               avancement.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file counts definitions and niy's in mclgui sources and
;;;;    prints a HTML and textual report.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-06 <PJB> Added this header.
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

(in-package :mclgui)

(defparameter *groups*
  (quote (("Points"       "point.lisp")
          ("Fonts"        "font.lisp")
          ("Menus"        "menu.lisp")
          ("Views"        "view-classes.lisp"
                          "view.lisp")
          ("Windows"      "window.lisp"
                          "window-menu-item.lisp")
          ("Dialogs"      "dialog.lisp"
                          "get-string-dialog.lisp"
                          "message-dialog.lisp"
                          "select-dialog.lisp"
                          "y-or-n-dialog.lisp")
          ("Dialog Items" "dialog-item.lisp"
                          "button-dialog-item.lisp"
                          "check-box-dialog-item.lisp"
                          "control-dialog-item.lisp"
                          "default-button-mixin.lisp"
                          "editable-text-dialog-item.lisp"
                          "focus-rect-mixin.lisp"
                          "pop-up-menu-dialog-item.lisp"
                          "radio-button-dialog-item.lisp"
                          "scroll-bar-dialog-item.lisp"
                          "sequence-dialog-item.lisp"
                          "static-text-dialog-item.lisp"
                          "table-dialog-item.lisp"
                          "scroller.lisp")
          ("Colors"       "color.lisp")
          ("Events"       "event.lisp"
                          "view-event.lisp"
                          "window-event.lisp"
                          "key-handler-mixin.lisp"
                          "eval.lisp"
                          "cursor.lisp")
          ("Clipboard"    "scrap.lisp")
          ("Apple Events" "application.lisp")
          ("Quickdraw"    "region.lisp"
                          "pattern.lisp"
                          "quickdraw.lisp"
                          "../pw-kernel/pw-graphics/controls/pw-graphics.lisp")
          ("Files"        "file.lisp")
          ("Divers"       "macros.lisp"
                          "mclgui.lisp"
                          "package.lisp"
                          "system.lisp"
                          "wrapper.lisp"
                          "objc-classes.lisp"
                          "notification.lisp"
                          "variables.lisp"))))

;; src/mclgui/dump.lisp
;; src/mclgui/graphics.lisp
;; src/mclgui/initialize.lisp
;; src/mclgui/test.lisp


(defun count-niy (tree)
  (cond
    ((eql 'niy tree) 1)
    ((atom tree)     0)
    (t (+ (count-niy (car tree))
          (count-niy (cdr tree))))))


(defmacro html (tag &body body)
  `(progn
     (format t "<~A~{ ~A~}>"
             ',(if (atom tag) tag (first tag))
             ,(if (atom tag) nil `(list ,@(rest  tag))))
     (prog1 (progn ,@body)
       (format t "</~A>~%"
               ',(if (atom tag) tag (first tag))))))


(defmacro percent-row ((percent width class) &body body)
  (let ((vpercent (gensym))
        (vwidth   (gensym))
        (vclass   (gensym)))
    `(let ((,vpercent ,percent)
           (,vwidth   ,width)
           (,vclass   ,class))
       (html "tr"
             (html ("td" "width=\"500px\"") ,@body)
             (html "td"
                   (html ("table" "border=\"0\"" "width=\"100%\"")
                         (html ("tr")
                               (html ("td" (format nil "width=\"~D%\"" (max 1 ,vpercent))
                                           (format nil "class=\"~A\"" ,vclass))
                                     (format t "&nbsp;"))
                               (when (< ,vpercent ,vwidth)
                                 (html ("td" (format nil "width=\"~D%\"" (max 1 (- ,vwidth ,vpercent)))
                                             (format nil "class=\"~A\"" "ltgray"))
                                       (format t "&nbsp;")))
                               (html ("td")
                                     (format t "&nbsp;")))))))))


(let* ((*default-pathname-defaults* #P"/home/pjb/works/patchwork/patchwork/src/mclgui/")
       (stats (loop
                :for (group . files) :in *groups*
                :collect (loop
                           :for file :in files
                           :for contents = (with-open-file (stream file)
                                             (loop
                                               :for sexp = (read stream nil stream)
                                               :until (eql sexp stream)
                                               :collect sexp))
                           :sum (count-if (lambda (sexp)
                                              (and (listp sexp)
                                               (symbolp (first sexp))
                                               (= 3 (mismatch "DEF" (string (first sexp))))))
                                          contents) :into definitions
                           :sum (count-niy contents) :into niy
                           :finally (return (list group definitions niy))))))

  (terpri) (terpri)
  (html "li"
        (princ "Extraire des sources de MCL l'API MCLGUI.") (terpri)
        (let ((max-fun (reduce (function max) stats :key (function second))))
          (flet ((percent (implemented)
                   (truncate (/ implemented max-fun) 1/100)))
            (html ("table" "width=\"90%\"")
                  (loop
                    :for index :from 1
                    :for (group definitions #|niy|#) :in stats
                    ;; :for implemented = (max 0 (- definitions niy))
                    :for status = "green" #-(and) (cond
                                                    ((zerop implemented)         "red")
                                                    ((= definitions implemented) "green")
                                                    (t                           "yellow"))
                    :do (percent-row ((percent definitions) (percent definitions) status)
                          (format t "~2D. ~A: ~D définitions</li>"
                                  index group definitions))))))
        (princ "Cette étape est finie.") (terpri))
  (princ "<br>") (terpri)
  (html "li"
        (princ "Implémenter l'API MCLGUI sur Mac OS X.") (terpri)
        (let ((max-fun (reduce (function max) stats :key (function second))))
          (flet ((percent (implemented)
                   (truncate (/ implemented max-fun) 1/100)))
            (html ("table" "width=\"90%\"")
                  (loop
                    :for index :from 1
                    :for (group definitions niy) :in stats
                    :for implemented = (max 0 (- definitions niy))
                    :for status =  (cond
                                     ((zerop implemented)         "red")
                                     ((= definitions implemented) "green")
                                     (t                           "yellow"))
                    :do  (percent-row ((percent implemented)
                                       (percent definitions)
                                       status)
                           (format t "~2D. ~A: ~D/~D fonctions implémentées</li>"
                                   index group implemented definitions)))))))
  (princ "<br>") (terpri) (terpri)

  (loop
    :for index :from 1
    :for (group definitions niy) :in stats
    :for implemented = (max 0 (- definitions niy))
    :do (format t "~2D. ~20A: ~3D/~3D fonctions implémentées [~3D%]~%"
                index group implemented definitions
                (truncate (/ implemented definitions) 1/100))
    :finally (terpri) (terpri))
  
  (values))



;;;; THE END ;;;;
