;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pattern.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file implements patterns.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-05 <PJB> Created.
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
(objcl:enable-objcl-reader-macros)


(defun bitmap-get-byte (bitmap x y)
  (loop
    :with width = (array-dimension bitmap 1)
    :with byte = 0
    :for i :from 7 :downto 0
    :while (< x width)
    :do (progn
          (print (list x width i y byte ))
          (when (plusp (aref bitmap y x))
            (setf (ldb (byte 1 i) byte) 1))
          (incf x))
    :finally (return byte)))


(defun bitmap-to-bytes (bitmap write-byte &optional (rowBytesAlignment 1))
  "
BITMAP:         A 2D array of bits.

WRITE-BYTE:     NIL, or a function taking a byte argument.

ROWBYTESALIGNMENT:
                The modulo the number of bytes per row must be congruent to.

DO:             If write-byte is NIL, just returns the results.

                Otherwise, call the function WRITE-BYTE with each byte from
                the bitmap in turn, row by row.

RETURN:         total-bytes; row-bytes; pixel-width; pixel-height.

TOTAL-BYTES:    number of times WRITE-BYTE has been or would be called.

ROW-BYTES:      number of bytes per row.

PIXEL-WIDTH:    Width  of the bitmap (array-dimension bitmap 1).

PIXEL-HEIGHT:   Height of the bitmap (array-dimension bitmap 0).
"
  (let* ((width         (array-dimension bitmap 1))
         (height        (array-dimension bitmap 0))
         (inRowBytes    (ceiling width 8))
         (outRowBytes   (* rowBytesAlignment (ceiling  width (* 8 rowBytesAlignment))))
         (totalBytes    (* outRowBytes height)))
    (when write-byte
      (flet ((get-byte (x y)
               (loop
                 :with byte = 0
                 :for i :from 7 :downto 0
                 :while (< x width)
                 :do (progn
                       (when (plusp (aref bitmap y x))
                         (setf (ldb (byte 1 i) byte) 1))
                       (incf x))
                 :finally (return byte))))
        (loop
          :for y :from 0 :below height
          :do (loop
                :for x :from 0 :below width :by 8
                :do (funcall write-byte (get-byte x y))
                :finally (loop :repeat (- outRowBytes inRowBytes) :do (funcall write-byte 0))))))
    (values totalBytes outRowBytes width height)))



(deftype bits8 ()
  "A Quickdraw Pattern data."
  '(array bit (8 8)))

(defun make-bits8 (&optional (initial-element 0))
  (make-array '(8 8) :element-type 'bit :initial-element initial-element))


(deftype bits16 ()
  "A Quickdraw cursor image or mask."
  '(array bit (16 16)))

(defun make-bits16 (&optional (initial-element 0))
  (make-array '(16 16) :element-type 'bit :initial-element initial-element))



;;;---------------------------------------------------------------------
;;;
;;; Pattern
;;;

(defclass pattern (wrapper)
  ((data :initarg :data :initform (make-bits8 0) :type bits8 :accessor pattern-data))
  (:documentation "A Quickdraw Pattern."))

(defmethod copy-object-from ((dst pattern) (src pattern))
  (setf (handle dst)       (handle src)
        (pattern-data dst) (alexandria:copy-array (pattern-data src)))
  dst)


(defmethod update-handle ((pattern pattern))
  (multiple-value-bind (totalBytes rowBytes width height) (bitmap-to-bytes (pattern-data pattern) nil)
    (declare (ignore totalBytes))
    (let* ((imagerep [[NSBitmapImageRep alloc]
                      initWithBitmapDataPlanes: *null*
                      pixelsWide: width
                      pixelsHigh: height
                      bitsPerSample: 1
                      samplesPerPixel: 1
                      hasAlpha: NO
                      isPlanar: NO
                      colorSpaceName: #$NSCalibratedBlackColorSpace
                      bytesPerRow: rowBytes
                      bitsPerPixel: 1])
           (image [[NSImage alloc] initWithSize:(ns:make-ns-size width height)]))
      (bitmap-to-bytes (pattern-data pattern)
                       (let ((data  [imagerep bitmapData])
                             (i     -1))
                         (lambda (byte) (setf (cffi:mem-aref data ':uint8 (incf i)) byte))))
      [image addRepresentation:[imagerep autorelease]]
      (setf (handle pattern) [NSColor colorWithPatternImage:[image autorelease]]))))

(defmethod initialize-instance :after ((pattern pattern) &key &allow-other-keys)
  (unless (handle pattern)
    (update-handle pattern)))

(defmethod print-object ((pattern pattern) stream)
  (print-parseable-object (pattern stream :type t :identity t)
                          (:data
                           (with-output-to-string (out)
                             (bitmap-to-bytes (pattern-data pattern)
                                              (lambda (byte)
                                                (format out "~%~8,'0B" byte))))))
  pattern)

(defun make-pattern (&rest bytes)
  (loop
    :with bits = (make-bits8)
    :for y :below 8
    :for byte = (or (pop bytes) 0)
    :do (loop
          :for x :below 8
          :for i = 1 :then (ash i 1)
          :do (when (plusp (logand i byte)) (setf (aref bits y x) 1)))
    :finally (return (make-instance 'pattern :data bits))))





(defun initialize/pattern ()
  (setf *black-pattern*  (make-instance 'pattern
                             :data (make-array '(8 8)
                                               :element-type 'bit
                                               :initial-contents '(#*11111111
                                                                   #*11111111
                                                                   #*11111111
                                                                   #*11111111
                                                                   #*11111111
                                                                   #*11111111
                                                                   #*11111111
                                                                   #*11111111)))
        *dark-gray-pattern*  (make-instance 'pattern
                                 :data (make-array '(8 8)
                                                   :element-type 'bit
                                                   :initial-contents '(#*10111011
                                                                       #*11111111
                                                                       #*11101110
                                                                       #*11111111
                                                                       #*10111011
                                                                       #*11111111
                                                                       #*11101110
                                                                       #*11111111)))
        *gray-pattern*  (make-instance 'pattern
                            :data (make-array '(8 8)
                                              :element-type 'bit
                                              :initial-contents '(#*10101010
                                                                  #*01010101
                                                                  #*10101010
                                                                  #*01010101
                                                                  #*10101010
                                                                  #*01010101
                                                                  #*10101010
                                                                  #*01010101)))
        *light-gray-pattern*  (make-instance 'pattern
                                  :data (make-array '(8 8)
                                                    :element-type 'bit
                                                    :initial-contents '(#*01000100
                                                                        #*00000000
                                                                        #*00010001
                                                                        #*00000000
                                                                        #*01000100
                                                                        #*00000000
                                                                        #*00010001
                                                                        #*00000000)))
        *white-pattern*  (make-instance 'pattern
                             :data (make-array '(8 8)
                                               :element-type 'bit
                                               :initial-contents '(#*00000000
                                                                   #*00000000
                                                                   #*00000000
                                                                   #*00000000
                                                                   #*00000000
                                                                   #*00000000
                                                                   #*00000000
                                                                   #*00000000)))))



;;;; THE END ;;;;
