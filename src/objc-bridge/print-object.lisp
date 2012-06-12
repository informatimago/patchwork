;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               print-object.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Print-object methods
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-05 <PJB> Extracted from bridge.lisp
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "OBJC-BRIDGE")



#-cocotron-objc
(defmethod print-object ((d ns::ns-decimal) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (unless (ccl:%null-ptr-p d)
      (format stream "exponent = ~d, length = ~s, is-negative = ~s, is-compact = ~s, mantissa = ~s" (ns::ns-decimal-exponent d) (ns::ns-decimal-length d) (ns::ns-decimal-is-negative d) (ns::ns-decimal-is-compact d) (ns::ns-decimal-mantissa d)))
    (describe-macptr-allocation-and-address d stream)))

#+darwin-target
(defmethod print-object ((a ns::aedesc) stream)
  (print-unreadable-object (a stream :type t :identity (ccl:%gcable-ptr-p a))
    (unless (ccl:%null-ptr-p a)
      (format stream "~s ~s"
              (ns::aedesc-descriptor-type a)
              (ns::aedesc-data-handle a)))
    (describe-macptr-allocation-and-address a stream)))



(defmethod print-object ((transform ns::ns-affine-transform-struct) stream)
  (print-unreadable-object (transform stream :type t :identity t)
    (format stream "~s ~s ~s ~s ~s ~s"
            (ns::ns-affine-transform-struct-m11 transform)
            (ns::ns-affine-transform-struct-m12 transform)
            (ns::ns-affine-transform-struct-m21 transform)
            (ns::ns-affine-transform-struct-m22 transform)
            (ns::ns-affine-transform-struct-tx transform)
            (ns::ns-affine-transform-struct-ty transform))
    (describe-macptr-allocation-and-address transform stream)))



(defmethod print-object ((r ns::ns-rect) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (unless #+ccl (ccl:%null-ptr-p r) #-ccl (niy null-ptr r)
      (flet ((maybe-round (x)
               (multiple-value-bind (q r) (round x)
                 (if (zerop r) q x))))
        (format stream "~s X ~s @ ~s,~s"
                (maybe-round (ns::ns-rect-width r))
                (maybe-round (ns::ns-rect-height r))
                (maybe-round (ns::ns-rect-x r))
                (maybe-round (ns::ns-rect-y r)))
        (describe-macptr-allocation-and-address r stream)))))



(defmethod print-object ((s ns::ns-size) stream)
  (flet ((maybe-round (x)
           (multiple-value-bind (q r) (round x)
             (if (zerop r) q x))))
    (unless #+ccl (ccl:%null-ptr-p s) #-ccl (niy null-ptr s)
      (print-unreadable-object (s stream :type t :identity t)
        (format stream "~s X ~s"
                (maybe-round (ns::ns-size-width s))
                (maybe-round (ns::ns-size-height s)))))
    (describe-macptr-allocation-and-address s stream)))


(defmethod print-object ((p ns::ns-point) stream)
  (flet ((maybe-round (x)
           (multiple-value-bind (q r) (round x)
             (if (zerop r) q x))))
    (print-unreadable-object (p stream :type t :identity t)
      (unless #+ccl (ccl:%null-ptr-p p) #-ccl (niy null-ptr-p p)
        (format stream "~s,~s"
                (maybe-round (ns::ns-point-x p))
                (maybe-round (ns::ns-point-y p))))
      (describe-macptr-allocation-and-address p stream))))


(defmethod print-object ((r ns::ns-range) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (unless #+ccl (ccl:%null-ptr-p r) #-ccl (null-ptr-p r)
      (format stream "~s/~s"
              (ns::ns-range-location r)
              (ns::ns-range-length r)))
    (describe-macptr-allocation-and-address r stream)))


;;;; THE END ;;;;
