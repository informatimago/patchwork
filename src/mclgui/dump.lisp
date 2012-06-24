;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dump.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-20 <PJB> Created.
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

(in-package "MCLGUI")



;; (define-method-combination append 
;;     :identity-with-one-argument t)
;; 
;; (defclass a () ())
;; (defclass b (a) ())
;; (defclass c (a) ())
;; (defclass d (b c) ())
;; (defclass e (d) ())
;; 
;; (defgeneric m (o)
;;   (:method-combination append)
;;   (:method append ((o a)) '(a))
;;   (:method append ((o b)) '(b))
;;   (:method append ((o c)) '(c))
;;   (:method append ((o d)) '(d))
;;   (:method append ((o e)) '(e)))
;; (m  (make-instance 'e))
;; --> (e d b c a)







(defun extract-slots-unique (ovar slots)
  "RETURN: A form building a plist of slot values."
  (cons 'list
        (loop
          :for slot :in slots
          :collect  (if (symbolp slot)
                        (intern (symbol-name slot) "KEYWORD")
                        `(quote ,(first slot)))
          :collect  (if (symbolp slot)
                        `(slot-value ,ovar ',slot)
                        (second slot)))))

(defmacro collect-slots (object &rest slots)
  (if (symbolp object)
      `((lambda (,object)
          (declare (ignorable ,object))
          ,(extract-slots object slots)) ,object)
      (destructuring-bind (ovar oval) object
        `((lambda (,ovar)
            (declare (ignorable ,ovar))
            ,(extract-slots object slots))  ,oval))))


(defgeneric object-slots (object)
  (:method-combination append)
  (:method append ((object t))
           (if (typep object 'standard-object)
               '()
               (list object))))

(defmethod object-slots append ((wrapper wrapper))
  (collect-slots wrapper
                 (:handle (princ-to-string (handle wrapper)))))

(defmethod object-slots append ((view simple-view))
  (collect-slots view
                 (:view-container       (dump-object-slots (slot-value view 'view-container)))
                 (:view-position        (point-to-list (slot-value view 'view-position)))
                 (:view-size            (point-to-list (slot-value view 'view-size)))
                 (:view-scroll-position (point-to-list (slot-value view 'view-scroll-position)))
                 view-nick-name view-font view-alist))

(defmethod object-slots append ((view view))
  (collect-slots view (:subviews (map 'list  (function dump-object-slots) (view-subviews view)))))

(defmethod object-slots append ((window window))
  (collect-slots window (:title (ignore-errors (window-title window)))))


(defvar *object-slots-processed* '())

(defgeneric dump-object-slots (object)
  (:method ((object t))
    object)
  (:method ((object standard-object))
    (or (gethash object *object-slots-processed*)
        (let ((odump (list (class-name (class-of object)))))
          (setf (gethash object *object-slots-processed*) odump
                (cdr odump) (append (object-slots object)
                                    (list (object-identity object))))
          odump))))

(defun dump (object)
  (let ((*object-slots-processed* (make-hash-table)))
    (dump-object-slots object)))



;; (setf *print-right-margin* 100)
;; (dump (make-instance 'view
;;                        :view-size (make-point 100 50)
;;                        :view-subviews (list (make-instance 'view
;;                                                 :view-position (make-point 0 0)
;;                                                 :view-size (make-point 50 50))
;;                                             (make-instance 'view
;;                                                 :view-position (make-point 50 0)
;;                                                 :view-size (make-point 50 50)))))








(defgeneric collect-views (view)
  (:method ((view simple-view))
    (list view))
  (:method ((view view))
    (cons view (mapcan (function collect-views)
                       (coerce (view-subviews view) 'list)))))



#||
(mapcar (lambda (v) [(ui::handle v) lockFocusIfCanDraw])
        (rest (ui::collect-views (first (windows)))))

(mapcar (lambda (v) [(ui::handle v) unlockFocus])
        (reverse (rest (ui::collect-views (first (windows))))))

(mapcar (lambda (w) (with-handle (h w) [h hasShadow])) (windows))
(mapcar (lambda (w) (with-handle (h w) [h isReleasedWhenClosed])) (windows :include-invisibles t))


||#

;;;; THE END ;;;;
