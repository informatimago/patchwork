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
      `((cl:lambda (,object)
          (declare (ignorable ,object))
          ,(extract-slots object slots)) ,object)
      (destructuring-bind (ovar oval) object
        `((cl:lambda (,ovar)
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

#+#.(cl:if (cl:find-package "PW") '(:and) '(:or))
(defmethod object-slots append ((patch pw:c-patch))
  (collect-slots patch (:active-mode (ignore-errors (active-mode patch)))))

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





(defgeneric map1-subviews (fun view)
  (:method (fun (view simple-view))
    (funcall fun view))
  (:method (fun (view view))
    (cons (funcall fun view)
          (map 'list
               (lambda (subview) (map1-subviews fun subview))
               (view-subviews view)))))



(defgeneric collect-views (view)
  (:method ((view simple-view))
    (list view))
  (:method ((view view))
    (cons view (mapcan (function collect-views)
                       (coerce (view-subviews view) 'list)))))


(defgeneric subview-tree (view)
  (:method ((view simple-view))
    (list view))
  (:method ((view view))
    (cons view (mapcar (function subview-tree)
                       (coerce (view-subviews view) 'list)))))

#-(and)(progn
         (mapcar (lambda (v) [(ui::handle v) lockFocusIfCanDraw])
                 (rest (ui::collect-views (first (windows)))))

         (mapcar (lambda (v) [(ui::handle v) unlockFocus])
                 (reverse (rest (ui::collect-views (first (windows))))))

         (mapcar (lambda (w) (with-handle (h w) [h hasShadow])) (windows))
         (mapcar (lambda (w) (with-handle (h w) [h isReleasedWhenClosed])) (windows :include-invisibles t))



         (let ((wins [[NSApplication sharedApplication] windows]))
           (dotimes (i [wins count] (terpri))
             (print (objcl:lisp-string [[wins objectAtIndex:i]title]))))



         (mapcar (lambda (nsw) (objcl:lisp-string [nsw title])) (nswindow-list))
         ("Test Window" "Test Window" "Test Window" "Test Window" "MN1" "start-swank.lisp" "Listener")
         (dolist (w (subseq (nswindow-list) 0 4))
           [w close]))

(defun nswindow-subviews (nswindow)
  (let ((views [[nswindow contentView] subviews])
        (list '()))
    (dotimes (i [views count] list)
      (push [views objectAtIndex:i] list))))

(defun nsview-subviews (nsview)
  (let ((views [nsview subviews])
        (list '()))
    (dotimes (i [views count] list)
      (push [views objectAtIndex:i] list))))

;; (nsview-view (first (nswindow-subviews (handle (first (windows)))))) 
;; #<test-view #xEFBA5B6>

(defun nswindow-list ()
 (let ((wins [[NSApplication sharedApplication] windows])
       (list '()))
   (dotimes (i [wins count] list)
     (push [wins objectAtIndex:i] list))))

(defun dump-nsview-subviews (nsview &optional (level 0))
  (format t "~V<~>~S ~S~%" level nsview (get-nsrect [nsview frame]))
  (format t "~V<~>~S~%" level (nsview-view nsview))
  (let ((nssubviews [nsview subviews]))
   (dotimes (i [nssubviews count] (values))
     (let ((subview  [nssubviews objectAtIndex:i])
           (level (+ 4 level)))
       (format t "~%")
       (format t "~V<~>frame      = ~S~%" level (get-nsrect [subview frame]))
       (format t "~V<~>bounds     = ~S~%" level (get-nsrect [subview bounds]))
       (dump-nsview-subviews subview level)))))

(defun dump-nswindow-subviews (nswindow)
  (format t "window: ~S ~S~%" nswindow (get-nsrect [nswindow frame]))
  (format t "        ~S~%" (nswindow-window nswindow))
  (format t "  contentView: ")
  (dump-nsview-subviews [nswindow contentView]))




(defun find-tree (item tree)
  (cond
    ((null tree) tree)
    ((equalp item tree) tree)
    ((atom tree) nil)
    (t (or (find-tree item (car tree))
           (find-tree item (cdr tree))))))



;;;; THE END ;;;;
