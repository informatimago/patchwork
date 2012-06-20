;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               font.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the Font API.
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

(in-package "MCLGUI")

(defun real-font (&optional font-spec)
  "
RETURN:         Whether the FONT-SPEC corresponds to a font or
                font-size that actually exists in the system, and is
                not a calculated font.

FONT-SPEC:      A font specification.  The default is the current
                font.
"
  ;; (cond 
  ;;  ((null font-spec)
  ;;   ;; still stupid but never called this way though is documented
  ;;   (with-port-macptr port     
  ;;     (let* ((font (#_getporttextfont port))             
  ;;            (size (#_getporttextsize port)))
  ;;       (#_realfont font size))))
  ;;  (t 
  ;;   (setq family (if (consp font-spec)
  ;;                  (dolist (x font-spec) (when (stringp x) (return x)))
  ;;                  (when (stringp font-spec) font-spec)))
  ;;   (when (and family
  ;;              (eq 0 (rlet ((fnum :integer))
  ;;                      (with-pstrs ((np family))
  ;;                        (#_GetFNum np fnum))
  ;;                      (%get-word fnum)))
  ;;              (not (equalp family (car (sys-font-spec)))))
  ;;     (return-from real-font nil))
  ;;   (multiple-value-setq (ff ms) (font-codes font-spec))
  ;;   (#_RealFont (ash ff -16) (logand ms #xff))))
  (niy real-font)
  nil)


#-(and) "

Font-face layout:
+------------------------------+---------------+---------------+
|      txFont                  |   txFace      |   unused      |
+------------------------------+---------------+---------------+
 31                          16 15            8 7             0

Mode-size layout:
+------------------------------+-------------------------------+
|      txMode                  |         txSize                |
+------------------------------+-------------------------------+
 31                          16 15                            0

"

(defvar *script-font-alist*      nil)
(defvar *font-name-number-alist* nil)

(defun font-name-from-number (font-code)
  (or (car (rassoc font-code *font-name-number-alist*))
      (%get-font-name font-code)))
  

(defun font-values (ff-code ms-code)
  "
RETURN: the five font values: font-code, size, mode, face and color.
"
  (let* ((ff-code   (or ff-code 0))
         (ms-code   (or ms-code 0))
         (font-code (point-v ff-code))
         (font      (font-name-from-number font-code))
         (mode      (xfer-mode-to-name (point-v ms-code)))
         (size      (point-h ms-code))
         (color     (ldb (byte 8 0) ff-code))
         (ff-code   (ldb (byte 8 8) ff-code))
         (face      (if (= ff-code 0)
                        :plain
                        (loop 
                          :for (style . code) :in *style-alist*
                          :when (plusp (logand code ff-code))
                          :collect style)))))
  (values font size mode face color))


(defun font-spec (ff-code ms-code)
  "
A font specification (font spec) is an atom or list of atoms
specifying one or more of the following: the font name, font size,
font styles, font color and transfer mode. They are more humanly
readable than font codes.  They can be translated into font codes
through the function FONTCODES.  Font codes represent font information
in a way that accesses the Macintosh Font Manager directly.  Since they
don’t need to be interpreted, they are significantly faster than font
specifications.  They can be translated into font specifications
explicitly through the function FONT-SPEC.

RETURN:         A font specification created from the font codes.

FF-CODE:        The font-face code, a 32-bit integer combining the
                encoded name of the font and its face (plain, bold,
                italic, etc).

MS-CODE:        The mode-size code, a 32-bit integer indicating the
                font mode (inclusive-or, exclusive-or, complemented,
                etc), and the font size.
"
  (multiple-value-bind (name size mode style color) (font-values ff-code ms-code)
    (list* name size mode
           (append (if (atom style)
                       (list style)
                       style)
                   (list (list :color-index color))))))


(defun string-width (string &optional font-spec)
  "

RETURN:         The width in pixel of the STRING, as if it was
                displayed in the font, size and style of the
                FONT-SPEC.

FONT-SPEC:      If not supplied, the current font is used.
"
  (niy string-width  string font-spec))


(defmacro grafport-write-string (string start end &optional ff ms color)
  (niy grafport-write-string string start end ff ms color)
  ;; `(grafport-write-unicode ,string ,start ,end ,ff ,ms ,color)
  `(niy grafport-write-string string start end ff ms color))


(defun current-font-codes ()
  "
RETURN: The font codes of the current font.
"
  (niy current-font-codes)
  (values 0 0))


(defun font-number-from-name (name)
  (niy font-number-from-name name)
  ;; (if (equalp item (car (sys-font-spec)))
  ;;             (setf font (ash (car *sys-font-codes*) -16))
  ;;             ;; in OS 8 its the real font-num - earlier it's 0 
  ;;             (let ((num (font-number-from-name item)))
  ;;               ;; so what do you do if it doesnt exist?
  ;;               (setf font (or num
  ;;                              (ash (car *sys-font-codes*) -16)))))
  0)


(defun font-info (&optional font-spec)
  "
RETURN:         four values that represent (in pixels) the ascent,
                descent, maximum width, and leading of font-spec.

FONT-SPEC:      If not supplied, the current font is used.

The ascent is the distance from the baseline to the highest ascender
of the font, the descent is the distance from the baseline to the
lowest descender of the font, the maximum width is that of the widest
character in the font, and the leading is the suggested spacing
between lines.  Only the font and font-size aspects of font-spec are
used in the calculation.  The font styles and transfer mode are not
significant.
"
  (multiple-value-bind (ff ms) (if font-spec
                                   (font-codes font-spec)
                                   (current-font-codes))
    (font-codes-info ff ms)))



(define-condition invalid-font-spec-error (error)
  ((font-spec :initarg :font-spec
              :reader invalid-font-spec)
   (reason    :initarg :reason
              :reader invalid-font-spec-reason)
   (option    :initarg :option
              :reader invalid-font-spec-option))
  (:report  (lambda (err stream)
              (format stream "Invalid font spec ~S, option ~S: ~A."
                      (invalid-font-spec err)
                      (invalid-font-spec-option err)
                      (case (invalid-font-spec-reason err)
                        (:duplicate-size      "duplicate sizes")
                        (:duplicate-name      "duplicate names")
                        (:duplicate-color     "duplicate colors")
                        (:invalid-color       "invalid color")
                        (:duplicate-text-mode "duplicate text-modes")
                        (:invalid-option      "invalid option" )
                        (otherwise (invalid-font-spec-reason err)))))))


(defun font-codes (font-spec &optional old-ff old-ms)
  "
Creates font codes from a font specification.

RETURN:         Four values: the font-face code, the mode-size code,
                the ff-mask, and the ms-mask. The two latter values
                are masks that tell which bits were specified in the
                font-face and mode-size codes, respectively.

FONT-SPEC:      A font specification.

OLD-FF:         The old font/face code. A font/face code is a 32-bit
                integer that combines the encoded name of the font and
                its face (plain, bold, italic, and so on). If there is
                an old-ff, its values are used if the new font
                specification specifies no value for either the font
                name or its face. If old-ff is nil or unspecified, it
                defaults to 0.

OLD-MS:         The old mode-size code. A mode-size code is a 32-bit
                integer that indicates the font mode (inclusive-or,
                exclusive-or, complemented, and so on) and the font
                size.  If there is an old-ms, its values are used if
                the new font specification specifies no value for
                either the font mode or its size. If old-ms is nil or
                unspecified, it defaults to 65536 (the code for a mode
                of :SRCOR and a size of 0).
"
  (unless font-spec
    (return-from font-codes (values old-ff old-ms 0 0)))
  (let* ((items      (ensure-list font-spec))
         (font       nil)
         (face       nil)
         (color      nil)
         (mode       nil)
         (size       nil)
         (font-mask  0)
         (face-mask  0)
         (color-mask 0)
         (mode-mask  0)
         (size-mask  0)
         (reset-style-p nil)
         (old-ff     (or old-ff 0))
         (old-ms     (or old-ms (make-point 0 (xfer-mode-arg :srcOr)))))
    (dolist (item items)
      (cond
        ((fixnump item)
         (when size 
           (error 'invalid-font-spec-error :font-spec font-spec 
                  :reason :duplicate-size  :option item))
         (setf size item
               size-mask -1))
        ((stringp item)
         (when font
           (error 'invalid-font-spec-error :font-spec font-spec 
                  :reason :duplicate-name  :option item))
         (setf font-mask -1)
         (setf font (font-number-from-name item)))
        ((consp item)
         (ecase (first item)
           (:color-index
            (when color
              (error 'invalid-font-spec-error :font-spec font-spec 
                     :reason :duplicate-color  :option item))
            (setf color (second item)
                  color-mask 255)
            (unless (and (fixnump color)
                         (<= 0 color 255))
              (error 'invalid-font-spec-error :font-spec font-spec 
                     :reason :invalid-color  :option item)))
           (:color
            (when color
              (error 'invalid-font-spec-error :font-spec font-spec 
                     :reason :duplicate-color  :option item))
            (setf color (color->ff-index (second item))
                  color-mask 255))))
        ((let ((temp (xfer-mode-arg item)))
           (when temp
             (if mode
                 (unless (eq item :plain)
                   (error 'invalid-font-spec-error :font-spec font-spec 
                          :reason :duplicate-text-mode  :option item))
                 (setf mode temp
                       mode-mask -1)))))
        ((let ((entry (assoc item *style-alist*)))
           (when entry
             (when (eql (car entry) :plain)
               (setf reset-style-p t
                     face-mask -1))
             (let ((code (cdr entry)))
               (setf face      (logior code (or face 0))
                     face-mask (logior code face-mask))))))
        (t
         (error 'invalid-font-spec-error :font-spec font-spec 
                :reason :invalid-option :option item))))
    (let ((font  (or font  (point-v old-ff)))
          (face  (if (and reset-style-p face)
                     face
                     (logior (or face 0) (ldb (byte 8 8) (point-h old-ff)))))
          (color (or color (ldb (byte 8 0) (point-h old-ff))))
          (mode  (or mode  (point-v old-ms)))
          (size  (or size  (point-h old-ms))))
      (values (make-point (dpb face (byte 8 8) color) font)
              (make-point size mode)
              (make-point (dpb face-mask (byte 8 8) color-mask) font-mask)
              (make-point size-mask mode-mask)))))


(defun font-codes-info (ff ms)
  "
RETURN:         Four values that represent (in pixels) the ascent,
                descent, maximum width, and leading of the font
                specified by FF-CODE and MS-CODE.

FF:             Font/Face code.

MS:             Mode/Size code.

The ascent is the distance from the baseline to the highest ascender
of the font, the descent is the distance from the baseline to the
lowest descender of the font, the maximum width is that of the widest
character in the font, and the leading is the suggested spacing
between lines. Only the font and font-size aspects of font-spec are
used in the calculation. The font styles and transfer mode are not
significant.
"
  (niy font-codes-info ff ms)
  (values 10 2 8 0))


(defun font-codes-line-height (ff ms)
  "
RETURN:         The line height for the font specified by FF and MS.

FF:             Font/Face code.

MS:             Mode/Size code.
"
  (multiple-value-bind (a d w l) (font-codes-info ff ms)
    (declare (ignore w))
    (+ a d l)))


(defun font-codes-string-width (string ff ms &optional
                                (start 0)
                                (end (length string)))
  "
RETURN:         The width in pixels of the substring of STRING from
                START to END using the font specified by FF and MS.

FF:             Font/Face code.

MS:             Mode/Size code.
"
  (check-type start fixnum "a start index in the string")
  (check-type end   fixnum "an end position in the string")
  (niy font-codes-string-width)
  (* 8 (- end start)))


(defun font-line-height (&optional font-spec)
  (multiple-value-bind (a d w l) (font-info font-spec)
    (declare (ignore w))
    (+ a d l)))


(defun merge-font-codes (old-ff-code old-ms-code ff-code ms-code &optional ff-mask ms-mask)
  "
DO:             The merge-font-codes function merges two font codes.

OLD-FF:         The old font/face code, expressed as a fixnum. A font/
                face code stores the encoded name of the font and its
                face (plain, bold, italic, and so on). If there is no
                old-ff, the value of old-ff is set to 0.

OLD-MS:         The old mode/size code, expressed as a fixnum. A mode/
                size code indicates the font mode (inclusive-or,
                exclusiveor, complemented, and so on) and the font
                size. If there is no old-ms, the value of old-ms is
                set to 0.

FF:             The new font/face code, expressed as a fixnum

MS:             The new mode/size code, expressed as a fixnum.

FF-MASK:        A mask that allows merge-font-codes to look only at
                certain bits of the font/face integer.

MS-MASK:        A mask that allows merge-font-codes to look only at
                certain bits of the mode/size integer.
"
  (values (if ff-mask
              (logior (logand ff-code ff-mask) (logand old-ff-code (lognot ff-mask)))
              ff-code)
          (if ms-mask
              (logior (logand ms-code ms-mask) (logand old-ms-code (lognot ms-mask)))
              ms-code)))



(defgeneric view-font (view)
  (:documentation "
RETURN:         The font specification used for drawing text in the window.

You should not write methods for this function; use VIEW-FONT-CODES
instead.
"))


(defgeneric set-view-font (view font-spec)
  (:documentation "
DO:             Sets the font of view to fontspec.

You should not write methods for this function; use SET-VIEWFONT-CODES
instead.
"))


(defgeneric view-font-codes (view)
  (:documentation "
RETURN:         Two values: the font/face code and mode/size code for
                view’s font.
"))


(defgeneric set-view-font-codes (view ff ms &optional ff-mask ms-mask)
  (:documentation "
DO:             Change the view font codes of view.  The font/face
                code is changed only in the bits that are set in
                FF-MASK.  The mode/size code is changed only in the
                bits that are set in MS-MASK.  These masks default to
                passing all bits of FF and MS.
"))


(defun sys-font-spec ()
  (font-spec (get-sys-font) 0))


(defun wrap-font (nsfont)
  (niy wrap-font nsfont)
  nsfont)

(defun initialize/font ()
  (niy initialize/font))


;;;; THE END ;;;;
