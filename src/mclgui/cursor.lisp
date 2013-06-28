;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cursor.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Cursors.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-18 <PJB> Created.
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

(defvar *current-cursor* nil)

(defclass cursor (wrapper)
 ((data     :initarg :data     :initform (make-bits16 0)  :type bits16 :accessor cursor-data)
   (mask     :initarg :mask     :initform (make-bits16 1)  :type bits16 :accessor cursor-mask)
   (hot-spot :initarg :hot-spot :initform (make-point 0 0) :type point  :accessor cursor-hot-spot)
   (name     :initarg :name     :initform "Cursor"         :type string :accessor cursor-name))
  (:documentation "A Quickdraw cursor"))


(defmethod print-object ((cursor cursor) stream)
  (declare (stepper disable))
  (print-parseable-object (cursor stream :type t :identity t)
                          name))


(defun cursor-premultiplied-data (cursor)
  "
RETURN:         A newly allocated bit16 containing the data of the
                cursor premultiplied by its mask (alpha).
"
  (let* ((data (cursor-data cursor))
         (mask (cursor-mask cursor))
         (prem (make-array (array-dimensions data)
                           :element-type (array-element-type data)
                           :initial-element 0)))
    (loop
      :for y :below (array-dimension data 0)
      :do (loop
            :for x :below (array-dimension data 1)
            :do (setf (aref prem x y) (* (aref data x y) (aref mask x y)))))
    prem))


(defmethod update-handle ((cursor cursor))
  (assert (equalp (array-dimensions (cursor-data cursor))  (array-dimensions (cursor-mask cursor))))
  (let ((prem (cursor-premultiplied-data cursor)))
    (multiple-value-bind (totalBytes rowBytes width height) (bitmap-to-bytes prem nil)
      (declare (ignore totalBytes))
      (let* ((imagerep [[NSBitmapImageRep alloc]
                        initWithBitmapDataPlanes: *null*
                        pixelsWide: width
                        pixelsHigh: height
                        bitsPerSample: 1
                        samplesPerPixel: 2
                        hasAlpha: YES
                        isPlanar: YES
                        colorSpaceName: #$NSCalibratedBlackColorSpace
                        bytesPerRow: rowBytes
                        bitsPerPixel: 1])
             (image [[NSImage alloc] initWithSize:(ns:make-ns-size width height)]))
        (cffi:with-foreign-object (planes '(:pointer :uint8) 5)
          [imagerep getBitmapDataPlanes:planes]
          (bitmap-to-bytes prem
                           (let ((data (cffi:mem-aref planes '(:pointer :uint8) 0))
                                 (i     -1))
                             (lambda (byte) (setf (cffi:mem-aref data ':uint8 (incf i)) byte))))
          (bitmap-to-bytes (cursor-mask cursor)
                           (let ((data (cffi:mem-aref planes '(:pointer :uint8) 1))
                                 (i     -1))
                             (lambda (byte) (setf (cffi:mem-aref data ':uint8 (incf i)) byte)))))
        [image addRepresentation:[imagerep autorelease]]
        (setf (handle cursor) [[NSCursor alloc]
                               initWithImage:[image autorelease]
                               hotSpot:(ns:make-ns-point (point-h (cursor-hot-spot cursor))
                                                         (point-v (cursor-hot-spot cursor)))])))))

(defmethod unwrap ((self cursor))
  (unwrapping self
    (or (handle self) (update-handle self))))


;; Not needed for now.  In anycase, NS cursors can be any size, and in
;; color, so in general we cannot convert them back to a Macintosh
;; cursor bitmap (16x16 black-and-white + mask).
;;
;; (defmethod wrap ((nscursor ns:ns-cursor))
;;   (let* ((representations [[nscursor image] representations])
;;          (bitmap-class [NSBitmapImageRep class])
;;          (rep (dotimes (i [representations count] [representations objectAtIndex:0])
;;                 (let ((rep [representations objectAtIndex:i]))
;;                   (when [rep isKindOfClass:bitmap-class]
;;                     (return rep))))))
;;     rep
;;     #-(and)    
;;     (make-instance 'cursor
;;       :handle nscursor
;;       :hot-spot (nspoint-to-point (get-nspoint [nscursor hotSpot]))
;;       :data
;;       :mask)))




(defgeneric set-cursor (cursor)
  (:documentation
   "
DO:             Sets the cursor to cursor.

CURSOR:         A cursor record or a 'CURS' resource ID.

NOTE:           If set-cursor is called from anywhere except within a
                WINDOW-UPDATE-CURSOR function, a function that is the
                value of *CURSORHOOK*, or a WITHOUT-INTERRUPTS special
                form, the event system’s background cursor handling
                immediately resets the cursor to some other shape.  If
                cursor is not of an acceptable type, then no action is
                taken.  To prevent the system from hanging at cursor
                update time, no error is signaled.
")
  (:method ((cursor t))
    (error "~S: Setting a cursor by resource ID is not implemented." 'set-cursor))
  (:method ((cursor cursor))
    ;; (format-trace "set-cursor" cursor)
    (setf *current-cursor* cursor)
    (unless (handle cursor)
      (update-handle cursor))
    (with-handle (nscursor cursor)
      [nscursor set])))


(defun update-cursor (&optional (hook *cursorhook*))
  "
The UPDATE-CURSOR function does the actual work of cursor handling.
If hook is a function or symbol, it is called with no arguments;
otherwise, SET-CURSOR is called with hook.

The UPDATE-CURSOR function is called periodically by the global
eventhandling system. It is not usually necessary to call this
function directly, but it may be called to make sure that the cursor
is correct at a particular time.

HOOK:           A function, symbol, or cursor. The default value is
                *CURSORHOOK*.
"
  ;; (format-trace "update-cursor" hook)
  (if (or (functionp hook) (symbolp hook))
    (funcall hook)
    (set-cursor hook)))


(defun call-with-cursor (cursor thunk)
  "
DO:             Calls the THUNK with the *cursorhook* bound to cursor.
                The cursor is temporarily set to *cursorhook* (or
                *cursorhook* is called if it's a function to set it).
                When THUNK is completed, the old cursor is reset (or
                if the old *cursorhook* was a function, it's called to
                reset the old cursor).
"
  (unwind-protect 
      (let ((*cursorhook* cursor))
        ;; (format-trace "call-with-cursor push" cursor)
        (if (or (functionp cursor) (symbolp cursor))
          (progn ; a function.
            [[NSCursor arrowCursor] push]
            (funcall cursor)) ; set up the cursor.
          (progn ; a cursor.
            (unless (handle cursor)
              (setf (handle cursor) (update-handle cursor)))
            [(handle cursor) push]))
        (funcall thunk)) ; eval the body.
    ;; The previous cursor is reverted automatically by pop, but  if
    ;; *cursorhook* is a function designator we call it to let it
    ;; know.
    ;; (format-trace "call-with-cursor pop" cursor)
    [NSCursor pop]
    (when (or (functionp *cursorhook*) (symbolp *cursorhook*))
      (funcall *cursorhook*))))


(defmacro with-cursor (cursor &body body)
  "
The WITH-CURSOR macro executes zero or more forms with
*CURSORHOOK* bound to cursor.

CURSOR:         A cursor structure, or a cursor hook function used to
                set the cursor.
"
  `(call-with-cursor ,cursor (lambda () ,@body)))


(defun make-cursor (name hs-x hs-y data mask &optional handle)
  (make-instance 'cursor
    :handle handle
    :name name
    :hot-spot (make-point hs-x hs-y)
    :data (make-array '(16 16) :element-type 'bit :initial-contents data)
    :mask (make-array '(16 16) :element-type 'bit :initial-contents mask)))


(defun initialize/cursor ()
  (setf *arrow-cursor*  (make-cursor "Arrow" 1 1
                                     #(#*0000000000000000
                                       #*0100000000000000
                                       #*0110000000000000
                                       #*0111000000000000
                                       #*0111100000000000
                                       #*0111110000000000
                                       #*0111111000000000
                                       #*0111111100000000
                                       #*0111111110000000
                                       #*0111111111000000
                                       #*0111111111100000
                                       #*0111011000000000
                                       #*0110011000000000
                                       #*0100001100000000
                                       #*0000001100000000
                                       #*0000000000000000)
                                     #(#*1100000000000000
                                       #*1110000000000000
                                       #*1111000000000000
                                       #*1111100000000000
                                       #*1111110000000000
                                       #*1111111000000000
                                       #*1111111100000000
                                       #*1111111110000000
                                       #*1111111111000000
                                       #*1111111111100000
                                       #*1111111111110000
                                       #*1111111111110000
                                       #*1111111100000000
                                       #*1110011110000000
                                       #*1100011110000000
                                       #*0000011110000000)
                                     [NSCursor arrowCursor])

        *i-beam-cursor* (make-cursor "I-Beam"  9 9
                                     #(#*0000000000000000
                                       #*0000010001000000
                                       #*0000001110000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000001110000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000001110000000
                                       #*0000010001000000
                                       #*0000000000000000)
                                     #(#*0000111011100000
                                       #*0000111111100000
                                       #*0000011111000000
                                       #*0000001110000000
                                       #*0000001110000000
                                       #*0000001110000000
                                       #*0000001110000000
                                       #*0000001110000000
                                       #*0000011111000000
                                       #*0000011111000000
                                       #*0000011111000000
                                       #*0000001110000000
                                       #*0000001110000000
                                       #*0000011111000000
                                       #*0000111111100000
                                       #*0000111011100000)
                                     (#/IBeamCursor ns:ns-cursor))

        *watch-cursor*  (make-cursor "Watch" 11 7
                                     #(#*0001111110000000
                                       #*0001111110000000
                                       #*0001111110000000
                                       #*0001111110000000
                                       #*0010000001000000
                                       #*0100001000100000
                                       #*0100001000100000
                                       #*0100001000110000
                                       #*0100111000110000
                                       #*0100000000100000
                                       #*0100000000100000
                                       #*0010000001000000
                                       #*0001111110000000
                                       #*0001111110000000
                                       #*0001111110000000
                                       #*0001111110000000)
                                     #(#*0011111111000000
                                       #*0011111111000000
                                       #*0011111111000000
                                       #*0011111111000000
                                       #*0111111111100000
                                       #*1111111111110000
                                       #*1111111111110000
                                       #*1111111111111000
                                       #*1111111111111000
                                       #*1111111111111000
                                       #*1111111111110000
                                       #*0111111111100000
                                       #*0011111111000000
                                       #*0011111111000000
                                       #*0011111111000000
                                       #*0011111111000000))

        *vertical-ps-cursor* (make-cursor "vertical-pane-separator" 8 7
                                          #(#*0000000000000000
                                            #*0000000010000000
                                            #*0000000111000000
                                            #*0000001111100000
                                            #*0000000010000000
                                            #*0000000010000000
                                            #*0011111111111100
                                            #*0000000000000000
                                            #*0011111111111100
                                            #*0000000010000000
                                            #*0000000010000000
                                            #*0000001111100000
                                            #*0000000111000000
                                            #*0000000010000000
                                            #*0000000000000000
                                            #*0000000000000000)
                                          #(#*0000000010000000
                                            #*0000000111000000
                                            #*0000001111100000
                                            #*0000011111110000
                                            #*0000000111000000
                                            #*0011111111111100
                                            #*0011111111111100
                                            #*0011111111111100
                                            #*0011111111111100
                                            #*0011111111111100
                                            #*0000000111000000
                                            #*0000011111110000
                                            #*0000001111100000
                                            #*0000000111000000
                                            #*0000000010000000
                                            #*0000000000000000))

        *horizontal-ps-cursor* (make-cursor "horizontal-pane-separator" 8 7
                                            #(#*0000000000000000
                                              #*0000000000000000
                                              #*0000001010000000
                                              #*0000001010000000
                                              #*0000001010000000
                                              #*0001001010010000
                                              #*0011001010011000
                                              #*0111111011111100
                                              #*0011001010011000
                                              #*0001001010010000
                                              #*0000001010000000
                                              #*0000001010000000
                                              #*0000001010000000
                                              #*0000001010000000
                                              #*0000000000000000
                                              #*0000000000000000)
                                            #(#*0000000000000000
                                              #*0000011111000000
                                              #*0000011111000000
                                              #*0001011111010000
                                              #*0011011111011000
                                              #*0111011111011100
                                              #*1111111111111110
                                              #*1111111111111111
                                              #*1111111111111110
                                              #*0111011111011100
                                              #*0011011111011000
                                              #*0001011111010000
                                              #*0000011111000000
                                              #*0000011111000000
                                              #*0000011111000000
                                              #*0000000000000000))

        *top-ps-cursor* (make-cursor "top-pane-separator" 7 2
                                     #(#*0000000000000000
                                       #*0011111111111100
                                       #*0000000000000000
                                       #*0011111111111100
                                       #*0000000100000000
                                       #*0000000100000000
                                       #*0000011111000000
                                       #*0000001110000000
                                       #*0000000100000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000)
                                     #(#*0011111111111100
                                       #*0011111111111100
                                       #*0011111111111100
                                       #*0011111111111100
                                       #*0011111111111100
                                       #*0000001110000000
                                       #*0000111111100000
                                       #*0000011111000000
                                       #*0000001110000000
                                       #*0000000100000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000
                                       #*0000000000000000))

        *bottom-ps-cursor* (make-cursor "bottom-pane-separator" 7 7
                                        #(#*0000000000000000
                                          #*0000000100000000
                                          #*0000001110000000
                                          #*0000011111000000
                                          #*0000000100000000
                                          #*0000000100000000
                                          #*0011111111111100
                                          #*0000000000000000
                                          #*0011111111111100
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000)
                                        #(#*0000001110000000
                                          #*0000011111000000
                                          #*0000111111100000
                                          #*0001111111110000
                                          #*0000001110000000
                                          #*0111111111111110
                                          #*0111111111111110
                                          #*0111111111111110
                                          #*0111111111111110
                                          #*0111111111111110
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000
                                          #*0000000000000000))

        *left-ps-cursor*  (make-cursor "left-pane-separator" 2 7
                                       #(#*0000000000000000
                                         #*0101000000000000
                                         #*0101000000000000
                                         #*0101000000000000
                                         #*0101000000000000
                                         #*0101001000000000
                                         #*0101001100000000
                                         #*0101111110000000
                                         #*0101001100000000
                                         #*0101001000000000
                                         #*0101000000000000
                                         #*0101000000000000
                                         #*0101000000000000
                                         #*0101000000000000
                                         #*0000000000000000
                                         #*0000000000000000)
                                       #(#*1111100000000000
                                         #*1111100000000000
                                         #*1111100000000000
                                         #*1111111000000000
                                         #*1111110100000000
                                         #*1111110010000000
                                         #*1111111111000000
                                         #*1111111111000000
                                         #*1111111111000000
                                         #*1111110010000000
                                         #*1111110100000000
                                         #*1111111000000000
                                         #*1111100000000000
                                         #*1111100000000000
                                         #*1111100000000000
                                         #*0000000000000000))
        
        *right-ps-cursor* (make-cursor "right-pane-separator" 7 7
                                       #(#*0000000000000000
                                         #*0000001010000000
                                         #*0000001010000000
                                         #*0000001010000000
                                         #*0000001010000000
                                         #*0001001010000000
                                         #*0011001010000000
                                         #*0111111010000000
                                         #*0011001010000000
                                         #*0001001010000000
                                         #*0000001010000000
                                         #*0000001010000000
                                         #*0000001010000000
                                         #*0000001010000000
                                         #*0000000000000000
                                         #*0000000000000000)
                                       #(#*0000011111000000
                                         #*0000011111000000
                                         #*0001011111000000
                                         #*0011011111000000
                                         #*0111011111000000
                                         #*1111111111000000
                                         #*1111111111000000
                                         #*1111111111000000
                                         #*0111011111000000
                                         #*0011011111000000
                                         #*0001011111000000
                                         #*0000011111000000
                                         #*0000011111000000
                                         #*0000011111000000
                                         #*0000011111000000
                                         #*0000000000000000))

        *cursorhook*     *arrow-cursor*
        *current-cursor* *arrow-cursor*)
    (update-cursor))


;;;; THE END ;;;;
