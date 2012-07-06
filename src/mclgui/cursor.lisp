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


(defclass cursor (wrapper)
  ((data     :initarg :data     :initform (make-bits16 0)  :type bits16 :accessor cursor-data)
   (mask     :initarg :mask     :initform (make-bits16 1)  :type bits16 :accessor cursor-mask)
   (hot-spot :initarg :hot-spot :initform (make-point 0 0) :type point  :accessor cursor-hot-spot))
  (:documentation "A Quickdraw cursor"))


(defun cursor-premultiplied-data (cursor)
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
        (ccl:rletz ((planes (:array (:* (:unsigned 8)) 5)))
          [imagerep getBitmapDataPlanes:planes]
          (bitmap-to-bytes prem
                           (let ((data (ccl::%get-ptr planes 0))
                                 (i     -1)))
                           (lambda (byte) (setf (ccl::%get-unsigned-byte data (incf i)) byte)))
          (bitmap-to-bytes (cursor-mask cursor)
                           (let ((data (ccl::%get-ptr planes 1))
                                 (i     -1)))
                           (lambda (byte) (setf (ccl::%get-unsigned-byte data (incf i)) byte))))
        [image addRepresentation:[imagerep autorelease]]
        (setf (handle cursor) [[NSCursor alloc]
                               initWithImage:[image autorelease]
                               hotSpot:(ns:make-ns-point (point-h (cursor-hot-spot cursor))
                                                         (point-v (cursor-hot-spot cursor)))])))))


(defmethod initialize-instance :after ((cursor cursor) &key &allow-other-keys)
  (unless (handle cursor)
    (update-handle cursor)))

(defun wrap-nscursor (nscursor)
  (let* ((representations [[nscursor image] representations])
         (bitmap-class [NSBitmapImageRep class])
         (rep (dotimes (i [representations count] [representations objectAtIndex:0])
                (let ((rep [representations objectAtIndex:i]))
                  (when [rep isKindOfClass:bitmap-class]
                    (return rep))))))
    rep
    #-(and)    
    (make-instance 'cursor
      :handle nscursor
      :hot-spot (nspoint-to-point (get-nspoint [nscursor hotSpot]))
      :data
      :mask)))

;; '[NSCursor IBeamCursor]
;; (macroexpand '(objc:send ns:ns-cursor '<ib>eam<c>ursor)(objc:send ns:ns-cursor '<ib>eam<c>ursor))
;; 
;; (wrap-nscursor [NSCursor IBeamCursor])
;; #<ns-bitmap-image-rep NSBitmapImageRep 0x35af080 Size={20, 24} ColorSpace=Generic RGB colorspace BPS=8 BPP=32 Pixels=20x24 Alpha=YES Planar=NO Format=0 CurrentBacking=<NSMutableData: 0x35aeff0> (#x35AF080)>




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
    (setf *current-cursor* cursor)
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
  (if (or (functionp hook) (symbolp hook))
    (funcall hook)
    (set-cursor hook)))


(defun call-with-cursor (cursor thunk)
  (unwind-protect 
      (let ((*cursorhook* cursor)) 
        (if (or (functionp cursor) (symbolp cursor))
          (progn ; a function.
            [[NSCursor arrowCursor] push]
            (funcall cursor)) ; set up the cursor.
          (progn ; a cursor.
            (unless (handle cursor)
              (setf (handle cursor) (unwrap cursor)))
            [(handle cursor) push]))
        (funcall thunk)) ; eval the body.
    ;; The previous cursor is reverted automatically by pop, but  if
    ;; *cursorhook* is a function designator we call it to let it
    ;; know.
    [NSCursor pop]
    (when (or (functionp *cursorhook*) (symbolp *cursorhook*))
      (funcall *cursorhook*))))


(defmacro with-cursor (cursor &body body)
  "
The WITH-CURSOR macro executes zero or more forms with
*CURSORHOOK* bound to cursor.

CURSOR:         A cursor structure.
"
  `(call-with-cursor ,cursor (lambda () ,@body)))



(defun initialize/cursor ()
  (setf *arrow-cursor*  (wrap-nscursor [NSCursor arrowCursor])
        ;; *i-beam-cursor* (wrap-nscursor [NSCursor IBeamCursor])
        *watch-cursor*  (make-instance 'cursor
                          :hot-spot (make-point 3 0)
                          :data (make-array '(16 16)
                                            :element-type 'bit
                                            :initial-contents '(#*0001111110000000
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
                                                                #*0001111110000000))
                          :mask (make-array '(16 16)
                                            :element-type 'bit
                                            :initial-contents '(#*0011111111000000
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
                                                                #*0011111111000000)))
        *cursorhook* *arrow-cursor*))


;;;; THE END ;;;;
